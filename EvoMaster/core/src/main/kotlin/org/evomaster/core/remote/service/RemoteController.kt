package org.evomaster.core.remote.service

import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException
import com.google.inject.Inject
import org.evomaster.client.java.controller.api.ControllerConstants
import org.evomaster.client.java.controller.api.dto.*
import org.evomaster.client.java.controller.api.dto.database.operations.DatabaseCommandDto
import org.evomaster.client.java.controller.api.dto.database.operations.InsertionResultsDto
import org.evomaster.client.java.controller.api.dto.database.operations.QueryResultDto
import org.evomaster.core.EMConfig
import org.evomaster.core.database.DatabaseExecutor
import org.evomaster.core.logging.LoggingUtil
import org.evomaster.core.remote.NoRemoteConnectionException
import org.evomaster.core.remote.SutProblemException
import org.evomaster.core.remote.TcpUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import javax.annotation.PostConstruct
import javax.annotation.PreDestroy
import javax.ws.rs.ProcessingException
import javax.ws.rs.client.Client
import javax.ws.rs.client.ClientBuilder
import javax.ws.rs.client.Entity
import javax.ws.rs.client.WebTarget
import javax.ws.rs.core.GenericType
import javax.ws.rs.core.MediaType
import javax.ws.rs.core.Response


/**
 * Class used to communicate with the remote RestController that does
 * handle the SUT.
 */
class RemoteController() : DatabaseExecutor {

    companion object {
        val log: Logger = LoggerFactory.getLogger(RemoteController::class.java)
    }

    lateinit var host: String
    var port: Int = 0

    private var computeSqlHeuristics = true

    private var extractSqlExecutionInfo = true


    @Inject
    private lateinit var config: EMConfig

    private var client: Client = ClientBuilder.newClient()

    constructor(config: EMConfig) : this(){
        this.config = config
        initialize()
    }

    constructor(host: String, port: Int, computeSqlHeuristics: Boolean, extractSqlExecutionInfo: Boolean, config: EMConfig = EMConfig()) : this() {
        if (computeSqlHeuristics && !extractSqlExecutionInfo)
            throw IllegalArgumentException("'extractSqlExecutionInfo' should be enabled when 'computeSqlHeuristics' is enabled")
        this.host = host
        this.port = port
        this.computeSqlHeuristics = computeSqlHeuristics
        this.extractSqlExecutionInfo = computeSqlHeuristics || extractSqlExecutionInfo
        this.config = config
    }

    constructor(host: String, port: Int, computeSqlHeuristics: Boolean) : this(host, port, computeSqlHeuristics, computeSqlHeuristics)

    @PostConstruct
    private fun initialize() {
        host = config.sutControllerHost
        port = config.sutControllerPort
        computeSqlHeuristics = config.heuristicsForSQL
        extractSqlExecutionInfo = config.extractSqlExecutionInfo
    }

    @PreDestroy
    private fun preDestroy() {
        close()
    }

    private fun getWebTarget(): WebTarget {
        return client.target("http://$host:$port" + ControllerConstants.BASE_PATH)
    }

    private fun makeHttpCall(lambda:  () -> Response) : Response{

        return  try{
            lambda.invoke()
        } catch (e: ProcessingException){
            when {
                TcpUtils.isOutOfEphemeralPorts(e) -> {
                    /*
                    This could happen if for any reason we run out of ephemeral ports.
                    In such a case, we wait X seconds, and try again, as OS might have released ports
                    meanwhile.
                    And while we are at it, let's release any hanging network resource
                    */
                    client.close() //make sure to release any resource
                    client = ClientBuilder.newClient()

                    TcpUtils.handleEphemeralPortIssue()

                    /*
                        [non-determinism-source] Man: this might lead to non-determinism
                     */
                    lambda.invoke()
                }
                TcpUtils.isRefusedConnection(e) -> {
                    //this is BAD!!! There isn't really much we can do here... :(
                    log.error("EvoMaster Driver process is no longer responding, refusing TCP connections." +
                            " Check if its process might have crashed. Also look at its logs")
                    throw e
                }
                TcpUtils.isStreamClosed(e) || TcpUtils.isEndOfFile(e) -> {
                    /*
                        TODO: there might be a potential issue here.
                        The GET on targets is not idempotent, as we collect the "first-seen" targets
                        only once. This should be handled here, somehow...
                     */
                    log.warn("EvoMaster Driver TCP connection is having issues: '${e.cause!!.message}'." +
                            " Let's wait a bit and try again.")
                    Thread.sleep(5_000)

                    /*
                        [non-determinism-source] Man: this might lead to non-determinism
                     */
                    lambda.invoke()
                }
                else -> throw e
            }
        }
    }

    fun close() {
        client.close()
    }

    private fun readAndCheckResponse(response: Response, msg: String) : Boolean{

        val dto = try {
            response.readEntity(object : GenericType<WrappedResponseDto<*>>() {})
        } catch (e: Exception) {
            handleFailedDtoParsing(e)
            null
        }

        return checkResponse(response, dto, msg)
    }

    private fun checkResponse(response: Response, dto: WrappedResponseDto<*>?, msg: String) : Boolean{
        if (response.statusInfo.family != Response.Status.Family.SUCCESSFUL  || dto?.error != null) {
            LoggingUtil.uniqueWarn(log, "$msg. HTTP status ${response.status}. Error: '${dto?.error}")
//            log.warn("{}. HTTP status {}. Error: '{}'", msg, response.status, dto?.error)
            return false
        }

        return true
    }

     private  fun <T> getData(dto: WrappedResponseDto<T>?) : T?{
         if (dto?.data == null) {
             log.warn("Missing DTO")
             return null
         }
         return dto.data
    }

    private fun <T> getDtoFromResponse(response: Response, type: GenericType<WrappedResponseDto<T>>) : WrappedResponseDto<T>?{

        if(response.mediaType == MediaType.TEXT_PLAIN_TYPE){
            //something weird is going on... possibly a bug in the Driver?

            val res = response.readEntity(String::class.java)
            log.error("Driver error. HTTP status ${response.status}. Error: $res")
            return null
        }


        val dto = try {
            response.readEntity(type)
        } catch (e: Exception) {
            handleFailedDtoParsing(e)
            null
        }

        return dto
    }


    fun getSutInfo(): SutInfoDto? {

        val response = makeHttpCall {
            getWebTarget()
                    .path(ControllerConstants.INFO_SUT_PATH)
                    .request(MediaType.APPLICATION_JSON_TYPE)
                    .get()
        }

        val dto = getDtoFromResponse(response, object : GenericType<WrappedResponseDto<SutInfoDto>>() {})

        if(!checkResponse(response, dto, "Failed to retrieve SUT info")){
            return null
        }

        return getData(dto)
    }


    fun getControllerInfo(): ControllerInfoDto? {

        val response = makeHttpCall {
            getWebTarget()
                    .path(ControllerConstants.CONTROLLER_INFO)
                    .request(MediaType.APPLICATION_JSON_TYPE)
                    .get()
        }

        val dto = getDtoFromResponse(response, object : GenericType<WrappedResponseDto<ControllerInfoDto>>() {})

        if(!checkResponse(response, dto, "Failed to retrieve EM controller info")){
            return null
        }

        return getData(dto)
    }

    private fun changeState(run: Boolean, reset: Boolean): Boolean {

        val response = try {
            makeHttpCall {
                getWebTarget()
                        .path(ControllerConstants.RUN_SUT_PATH)
                        .request()
                        .put(Entity.json(SutRunDto(run, reset, computeSqlHeuristics, extractSqlExecutionInfo)))
            }
        } catch (e: Exception) {
            log.warn("Failed to connect to SUT: ${e.message}")
            return false
        }

        val dto = getDtoFromResponse(response, object : GenericType<WrappedResponseDto<Any>>() {})

        return checkResponse(response, dto, "Failed to change running state of the SUT")
    }

    /*
        Starting implies a clean reset state.
        Reset needs SUT to be up and running.
        If SUT already running, no need to restart it, we can reset its state.
        So, start and reset have same functionality here.
    */

    fun startSUT() = changeState(true, true)

    fun stopSUT() = changeState(false, false)

    fun resetSUT() = startSUT()

    fun checkConnection() {

        val response = try {
            getWebTarget()
                    .path(ControllerConstants.CONTROLLER_INFO)
                    .request(MediaType.APPLICATION_JSON_TYPE)
                    .get()
        } catch (e: Exception) {
            //FIXME proper exception checking
            throw NoRemoteConnectionException(port, host)
        }

        response.close()
    }

    fun startANewSearch(): Boolean {

        val response = makeHttpCall {
            getWebTarget()
                    .path(ControllerConstants.NEW_SEARCH)
                    .request()
                    .post(Entity.entity("{\"newSearch\":true}", MediaType.APPLICATION_JSON_TYPE))
        }

        return readAndCheckResponse(response, "Failed to inform SUT of new search")
    }

    fun getTestResults(ids: Set<Int> = setOf(), ignoreKillSwitch: Boolean = false): TestResultsDto? {

        val queryParam = ids.joinToString(",")

        val response = makeHttpCall {
            getWebTarget()
                    .path(ControllerConstants.TEST_RESULTS)
                    .queryParam("ids", queryParam)
                    .queryParam("killSwitch", !ignoreKillSwitch && config.killSwitch)
                    .request(MediaType.APPLICATION_JSON_TYPE)
                    .get()
        }

        val dto = getDtoFromResponse(response, object : GenericType<WrappedResponseDto<TestResultsDto>>() {})

        if(!checkResponse(response, dto, "Failed to retrieve target coverage for $queryParam")){
            return null
        }

        return getData(dto)
    }

    fun registerNewAction(actionDto: ActionDto) : Boolean{

        val response = makeHttpCall {
            getWebTarget()
                    .path(ControllerConstants.NEW_ACTION)
                    .request()
                    .put(Entity.entity(actionDto, MediaType.APPLICATION_JSON_TYPE))
        }

        return readAndCheckResponse(response, "Failed to register new action")
    }

    override fun executeDatabaseCommand(dto: DatabaseCommandDto): Boolean {

        log.trace("Going to execute database command. Command:{} , Insertion.size={}",dto.command,dto.insertions?.size ?: 0)

        val response = makeHttpCall {
            getWebTarget()
                    .path(ControllerConstants.DATABASE_COMMAND)
                    .request()
                    .post(Entity.entity(dto, MediaType.APPLICATION_JSON_TYPE))
        }

        /*
           [non-determinism-source] Man: this might lead to non-determinism
        */
        if (!wasSuccess(response)) {

            LoggingUtil.uniqueWarn(log, "Failed to execute database command. HTTP status: {}.", response.status)

            if(response.mediaType == MediaType.TEXT_PLAIN_TYPE){
                //something weird is going on... possibly a bug in the Driver?

                val res = response.readEntity(String::class.java)
                log.error("Database command failure, HTTP status ${response.status}: $res")
                return false
            }

            val responseDto = try {
                response.readEntity(object : GenericType<WrappedResponseDto<*>>() {})
            } catch (e: Exception) {
                handleFailedDtoParsing(e)
                return false
            }

            if(responseDto.error != null) {
                //this can happen if we do not handle all constraints
                LoggingUtil.uniqueWarn(log, "Error message: " + responseDto.error)
            }

            /*
                TODO refactor this method once we support most of SQL handling, and do not need
                to have uniqueWarn any longer
             */

            return false
        }

        return true
    }

    override fun executeDatabaseCommandAndGetQueryResults(dto: DatabaseCommandDto): QueryResultDto? {
        return executeDatabaseCommandAndGetResults(dto, object : GenericType<WrappedResponseDto<QueryResultDto>>() {})
    }

    override fun executeDatabaseInsertionsAndGetIdMapping(dto: DatabaseCommandDto): InsertionResultsDto? {
        return executeDatabaseCommandAndGetResults(dto, object : GenericType<WrappedResponseDto<InsertionResultsDto>>() {})
    }

    private fun <T> executeDatabaseCommandAndGetResults(dto: DatabaseCommandDto, type: GenericType<WrappedResponseDto<T>>): T?{

        val response = makeHttpCall {
            getWebTarget()
                    .path(ControllerConstants.DATABASE_COMMAND)
                    .request()
                    .post(Entity.entity(dto, MediaType.APPLICATION_JSON_TYPE))
        }

        val dto = getDtoFromResponse(response, type)

        if (!checkResponse(response, dto, "Failed to execute database command")) {
            return null
        }

        return dto?.data
    }


    private fun wasSuccess(response: Response?): Boolean {
        return response?.statusInfo?.family?.equals(Response.Status.Family.SUCCESSFUL) ?: false
    }

    private fun handleFailedDtoParsing(exception: Exception){

        if(exception is ProcessingException && exception.cause is UnrecognizedPropertyException){

            val version = this.javaClass.`package`?.implementationVersion
                    ?: "(cannot determine, likely due to EvoMaster being run directly from IDE and not as a packaged uber jar)"

            throw SutProblemException("There is a mismatch between the DTO that EvoMaster Driver is " +
                    "sending and what the EvoMaster Core process (this process) is expecting to receive. " +
                    "Are you sure you are using the same matching versions? This EvoMaster Core " +
                    "process version is: $version")
        } else {
            log.warn("Failed to parse dto", exception)
        }
    }
}