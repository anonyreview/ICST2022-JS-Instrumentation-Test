package org.evomaster.core

import joptsimple.BuiltinHelpFormatter
import joptsimple.OptionDescriptor
import joptsimple.OptionParser
import joptsimple.OptionSet
import org.evomaster.client.java.controller.api.ControllerConstants
import org.evomaster.core.logging.LoggingUtil
import org.evomaster.core.output.OutputFormat
import org.evomaster.core.search.impact.impactinfocollection.GeneMutationSelectionMethod
import org.slf4j.LoggerFactory
import java.net.MalformedURLException
import java.net.URL
import java.nio.file.Files
import java.nio.file.InvalidPathException
import java.nio.file.Paths
import java.util.logging.Logger
import kotlin.reflect.KMutableProperty
import kotlin.reflect.jvm.javaType

typealias PercentageAsProbability = EMConfig.Probability

/**
 * Class used to hold all the main configuration properties
 * of EvoMaster.
 *
 */
class EMConfig {

    /*
        Code here does use the JOptSimple library

        https://pholser.github.io/jopt-simple/
     */

    companion object {

        private val log = LoggerFactory.getLogger(EMConfig::class.java)

        fun validateOptions(args: Array<String>): OptionParser {

            val config = EMConfig()

            val parser = getOptionParser()
            val options = parser.parse(*args)

            if (!options.has("help")) {
                //actual validation is done here
                config.updateProperties(options)
            }

            return parser
        }

        /**
         * Having issue with types/kotlin/reflection...
         * Therefore, need custom output formatting.
         * However, easier way (for now) is to just override
         * what we want to change
         *
         *  TODO: groups and ordering
         */
        private class MyHelpFormatter : BuiltinHelpFormatter(80, 2) {
            override fun extractTypeIndicator(descriptor: OptionDescriptor): String? {
                return null
            }
        }

        /**
         * Get all available "console options" for the annotated properties
         */
        fun getOptionParser(): OptionParser {

            val defaultInstance = EMConfig()

            val parser = OptionParser()

            parser.accepts("help", "Print this help documentation")
                    .forHelp()

            getConfigurationProperties().forEach { m ->
                /*
                    Note: here we could use typing in the options,
                    instead of converting everything to string.
                    But it looks bit cumbersome to do it in Kotlin,
                    at least for them moment

                    Until we make a complete MyHelpFormatter, here
                    for the types we "hack" the default one, ie,
                    we set the type description to "null", but then
                    the argument description will contain the type
                 */

                val argTypeName = m.returnType.toString()
                        .run { substring(lastIndexOf('.') + 1) }

                parser.accepts(m.name, getDescription(m).toString())
                        .withRequiredArg()
                        .describedAs(argTypeName)
                        .defaultsTo(m.call(defaultInstance).toString())
            }

            parser.formatHelpWith(MyHelpFormatter())

            return parser
        }

        class ConfigDescription(
                val text: String,
                val constraints: String,
                val enumExperimentalValues: String,
                val enumValidValues: String,
                val experimental: Boolean
        ) {
            override fun toString(): String {
                var description = text
                if (constraints.isNotBlank()) {
                    description += " [Constraints: $constraints]."
                }
                if (enumValidValues.isNotBlank()) {
                    description += " [Values: $enumValidValues]."
                }
                if (enumExperimentalValues.isNotBlank()) {
                    description += " [Experimental Values: $enumExperimentalValues]."
                }




                if (experimental) {
                    /*
                    TODO: For some reasons, coloring is not working here.
                    Could open an issue at:
                    https://github.com/jopt-simple/jopt-simple
                    */
                    //description = AnsiColor.inRed("EXPERIMENTAL: $description")
                    description = "EXPERIMENTAL: $description"
                }
                return description
            }
        }

        fun getDescription(m: KMutableProperty<*>): ConfigDescription {

            val cfg = (m.annotations.find { it is Cfg } as? Cfg)
                    ?: throw IllegalArgumentException("Property ${m.name} is not annotated with @Cfg")

            val text = cfg.description.trim().run {
                when {
                    isBlank() -> "No description."
                    !endsWith(".") -> "$this."
                    else -> this
                }
            }

            val min = (m.annotations.find { it is Min } as? Min)?.min
            val max = (m.annotations.find { it is Max } as? Max)?.max
            val probability = m.annotations.find { it is Probability }
            val url = m.annotations.find { it is Url }
            val regex = (m.annotations.find { it is Regex } as? Regex)?.regex

            var constraints = ""
            if (min != null || max != null || probability != null || url != null || regex != null) {
                if (min != null) {
                    constraints += "min=$min"
                }
                if (max != null) {
                    if (min != null) constraints += ", "
                    constraints += "max=$max"
                }
                if (probability != null) {
                    constraints += "probability 0.0-1.0"
                }
                if (url != null) {
                    constraints += "URL"
                }
                if (regex != null) {
                    constraints += "regex $regex"
                }
            }

            var experimentalValues =""
            var validValues = ""
            val returnType = m.returnType.javaType as Class<*>

            if (returnType.isEnum) {
                val elements = returnType.getDeclaredMethod("values")
                        .invoke(null) as Array<*>
                val experimentElements= elements.filter{ it is WithExperimentalOptions && it.isExperimental()}
                val validElements= elements.filter{ it !is WithExperimentalOptions || !it.isExperimental()}
                experimentalValues = experimentElements.joinToString(", ")
                validValues = validElements.joinToString(", ")
            }

            val experimental = (m.annotations.find { it is Experimental } as? Experimental)

            val cd = ConfigDescription(text, constraints, experimentalValues ,validValues, experimental != null)

            return cd
        }


        fun getConfigurationProperties(): List<KMutableProperty<*>> {
            return EMConfig::class.members
                    .filterIsInstance(KMutableProperty::class.java)
                    .filter { it.annotations.any { it is Cfg } }
        }
    }

    /**
     * Update the values of the properties based on the options
     * chosen on the command line
     *
     *
     * @throws IllegalArgumentException if there are constraint violations
     */
    fun updateProperties(options: OptionSet) {

        getConfigurationProperties().forEach { m ->

            updateProperty(options, m)

            checkPropertyConstraints(m)
        }

        checkMultiFieldConstraints()

        handleDeprecated()
    }


    private fun handleDeprecated(){
        /*
            TODO If this happens often, then should use annotations.
            eg, could handle specially in Markdown all the deprecated fields
         */
        if(testSuiteFileName.isNotBlank()){
            log.warn("Using deprecated option 'testSuiteFileName'")
            outputFilePrefix = testSuiteFileName
            outputFileSuffix = ""
            testSuiteFileName = ""
        }
    }

    fun checkMultiFieldConstraints() {
        /*
            Each option field might have specific constraints, setup with @annotations.
            However, there can be multi-field constraints as well.
            Those are defined here.
            They can be check only once all fields have been updated
         */

        if (blackBox && !bbExperiments) {

            if(problemType == ProblemType.DEFAULT){
                LoggingUtil.uniqueWarn(log, AnsiColor.inRed("WARNING: you are doing Black-Box testing, but you did not specify the" +
                        " 'problemType'. The system will default to RESTful API testing."))
                problemType = ProblemType.REST
            }

            if (problemType == ProblemType.REST && bbSwaggerUrl.isNullOrBlank()) {
                throw IllegalArgumentException("In black-box mode for REST APIs, you must set the bbSwaggerUrl option")
            }
            if(problemType == ProblemType.GRAPHQL && bbTargetUrl.isNullOrBlank()){
                throw java.lang.IllegalArgumentException("In black-box mode for GraphQL APIs, you must set the bbTargetUrl option")
            }
            if (outputFormat == OutputFormat.DEFAULT) {
                /*
                    TODO in the future, once we support POSTMAN outputs, we should default it here
                 */
                throw IllegalArgumentException("In black-box mode, you must specify a value for the outputFormat option different from DEFAULT")
            }
        }

        if (!blackBox && bbExperiments) {
            throw IllegalArgumentException("Cannot setup bbExperiments without black-box mode")
        }

        if(!blackBox && ratePerMinute > 0){
            throw IllegalArgumentException("ratePerMinute is used only for black-box testing")
        }

        if(blackBox && ratePerMinute <=0){
            LoggingUtil.getInfoLogger().warn("You have not setup 'ratePerMinute'. If you are doing testing of" +
                    " a remote service which you do not own, you might want to put a rate-limiter to prevent" +
                    " EvoMaster from bombarding such service with HTTP requests.")
        }

        when (stoppingCriterion) {
            StoppingCriterion.TIME -> if (maxActionEvaluations != defaultMaxActionEvaluations) {
                throw IllegalArgumentException("Changing number of max actions, but stopping criterion is time")
            }
            StoppingCriterion.FITNESS_EVALUATIONS -> if (maxTimeInSeconds != defaultMaxTimeInSeconds ||
                    maxTime != defaultMaxTime) {
                throw IllegalArgumentException("Changing max time, but stopping criterion is based on fitness evaluations")
            }
        }

        if (shouldGenerateSqlData() && !heuristicsForSQL) {
            throw IllegalArgumentException("Cannot generate SQL data if you not enable " +
                    "collecting heuristics with 'heuristicsForSQL'")
        }

        if (heuristicsForSQL && !extractSqlExecutionInfo) {
            throw IllegalArgumentException("Cannot collect heuristics SQL data if you not enable " +
                    "extracting SQL execution info with 'extractSqlExecutionInfo'")
        }

        if (enableTrackEvaluatedIndividual && enableTrackIndividual) {
            throw IllegalArgumentException("When tracking EvaluatedIndividual, it is not necessary to track individual")
        }

        //resource related parameters
        if(problemType != ProblemType.DEFAULT) {
            if ((resourceSampleStrategy != ResourceSamplingStrategy.NONE || (probOfApplySQLActionToCreateResources > 0.0) || doesApplyNameMatching || probOfEnablingResourceDependencyHeuristics > 0.0 || exportDependencies)
                    && (problemType != ProblemType.REST || algorithm != Algorithm.MIO)) {
                throw IllegalArgumentException("Parameters (${
                    arrayOf("resourceSampleStrategy", "probOfApplySQLActionToCreateResources", "doesApplyNameMatching", "probOfEnablingResourceDependencyHeuristics", "exportDependencies")
                            .filterIndexed { index, _ ->
                                (index == 0 && resourceSampleStrategy != ResourceSamplingStrategy.NONE) ||
                                        (index == 1 && (probOfApplySQLActionToCreateResources > 0.0)) ||
                                        (index == 2 && doesApplyNameMatching) ||
                                        (index == 3 && probOfEnablingResourceDependencyHeuristics > 0.0) ||
                                        (index == 4 && exportDependencies)
                            }.joinToString(" and ")
                }) are only applicable on REST problem (but current is $problemType) with MIO algorithm (but current is $algorithm).")
            }
        }

        /*
            resource-mio and sql configuration
            TODO if required
         */
//        if (resourceSampleStrategy != ResourceSamplingStrategy.NONE && (heuristicsForSQL || generateSqlDataWithSearch || generateSqlDataWithDSE || geneMutationStrategy == GeneMutationStrategy.ONE_OVER_N)) {
//            throw IllegalArgumentException("Resource-mio does not support SQL strategies for the moment")
//        }

        //archive-based mutation
//        if (adaptiveGeneSelectionMethod != GeneMutationSelectionMethod.NONE && algorithm != Algorithm.MIO) {
//            throw IllegalArgumentException("GeneMutationSelectionMethod is only applicable with MIO algorithm (but current is $algorithm)")
//        }

        if (adaptiveGeneSelectionMethod != GeneMutationSelectionMethod.NONE && probOfArchiveMutation > 0 && !weightBasedMutationRate)
            throw IllegalArgumentException("When applying adaptive gene selection, weight-based mutation rate should be enabled")

        if (probOfArchiveMutation > 0 && !enableTrackEvaluatedIndividual)
            throw IllegalArgumentException("Archive-based solution is only applicable when enable of tracking of EvaluatedIndividual.")

        if (doCollectImpact && !enableTrackEvaluatedIndividual)
            throw IllegalArgumentException("Impact collection should be applied together with tracking EvaluatedIndividual")

        if (baseTaintAnalysisProbability > 0 && !useMethodReplacement) {
            throw IllegalArgumentException("Base Taint Analysis requires 'useMethodReplacement' option")
        }

        if ((outputFilePrefix.contains("-") || outputFileSuffix.contains("-"))
                    && outputFormat.isJavaOrKotlin()) { //TODO also for C#?
             throw IllegalArgumentException("In JVM languages, you cannot use the symbol '-' in test suite file name")
        }

        if (seedTestCases && seedTestCasesPath.isNullOrBlank()) {
            throw IllegalArgumentException("When using the seedTestCases option, you must specify the file path of the test cases with the seedTestCasesPath option")
        }

        // Clustering constraints: the executive summary is not really meaningful without the clustering
        if(executiveSummary && testSuiteSplitType != TestSuiteSplitType.CLUSTER){
            executiveSummary = false
            LoggingUtil.getInfoLogger().warn("The option to turn on Executive Summary is only meaningful when clustering is turned on (--testSuiteSplitType CLUSTERING). " +
                    "The option has been deactivated for this run, to prevent a crash.")
            //throw IllegalArgumentException("The option to turn on Executive Summary is only meaningful when clustering is turned on (--testSuiteSplitType CLUSTERING).")
        }
    }

    private fun checkPropertyConstraints(m: KMutableProperty<*>) {
        val parameterValue = m.getter.call(this).toString()

        //check value against constraints on its field, if any
        m.annotations.find { it is Min }?.also {
            it as Min
            if (parameterValue.toDouble() < it.min) {
                throw IllegalArgumentException("Failed to handle Min ${it.min} constraint for" +
                        " parameter '${m.name}' with value $parameterValue")
            }
        }

        m.annotations.find { it is Max }?.also {
            it as Max
            if (parameterValue.toDouble() > it.max) {
                throw IllegalArgumentException("Failed to handle Max ${it.max} constraint for" +
                        " parameter '${m.name}' with value $parameterValue")
            }
        }

        m.annotations.find { it is Probability }?.also {
            it as Probability
            val p = parameterValue.toDouble()
            if (p < 0 || p > 1) {
                throw IllegalArgumentException("Failed to handle probability constraint for" +
                        " parameter '${m.name}' with value $parameterValue. The value must be in [0,1].")
            }
        }

        m.annotations.find { it is Url }?.also {
            if (!parameterValue.isNullOrBlank()) {
                try {
                    URL(parameterValue)
                } catch (e: MalformedURLException) {
                    throw IllegalArgumentException("Parameter '${m.name}' with value $parameterValue is" +
                            " not a valid URL: ${e.message}")
                }
            }
        }

        m.annotations.find { it is Regex }?.also {
            it as Regex
            if (!parameterValue.matches(kotlin.text.Regex(it.regex))) {
                throw IllegalArgumentException("Parameter '${m.name}' with value $parameterValue is" +
                        " not matching the regex: ${it.regex}")
            }
        }

        m.annotations.find { it is Folder }?.also{
            val path = try{
                Paths.get(parameterValue).toAbsolutePath()
            } catch(e: InvalidPathException){
                throw IllegalArgumentException("Parameter '${m.name}' is not a valid FS path: ${e.message}")
            }

            if(Files.exists(path) && ! Files.isWritable(path)){
                throw IllegalArgumentException("Parameter '${m.name}' refers to a folder that already" +
                        " exists, but that cannot be written to: $path")
            }
            if(Files.exists(path) && ! Files.isDirectory(path)){
                throw IllegalArgumentException("Parameter '${m.name}' refers to a file that already" +
                        " exists, but that it is not a folder: $path")
            }
        }

        m.annotations.find { it is FilePath }?.also{
            val path = try{
                Paths.get(parameterValue).toAbsolutePath()
            } catch(e: InvalidPathException){
                throw IllegalArgumentException("Parameter '${m.name}' is not a valid FS path: ${e.message}")
            }

            if(Files.exists(path) && ! Files.isWritable(path)){
                throw IllegalArgumentException("Parameter '${m.name}' refers to a file that already" +
                        " exists, but that cannot be written/replace to: $path")
            }
            if(Files.exists(path) && Files.isDirectory(path)){
                throw IllegalArgumentException("Parameter '${m.name}' refers to a file that is instead an" +
                        " existing folder: $path")
            }
        }
    }

    private fun updateProperty(options: OptionSet, m: KMutableProperty<*>) {
        val opt = options.valueOf(m.name)?.toString()
                ?: throw IllegalArgumentException("Value not found for property ${m.name}")

        val returnType = m.returnType.javaType as Class<*>

        /*
                TODO: ugly checks. But not sure yet if can be made better in Kotlin.
                Could be improved with isSubtypeOf from 1.1?
                http://stackoverflow.com/questions/41553647/kotlin-isassignablefrom-and-reflection-type-checks
             */

        //update value, but only if it was in the specified options.
        //WARNING: without this check, it would reset to default for fields not in "options"
        if (options.has(m.name)) {
            try {
                if (Integer.TYPE.isAssignableFrom(returnType)) {
                    m.setter.call(this, Integer.parseInt(opt))

                } else if (java.lang.Long.TYPE.isAssignableFrom(returnType)) {
                    m.setter.call(this, java.lang.Long.parseLong(opt))

                } else if (java.lang.Double.TYPE.isAssignableFrom(returnType)) {
                    m.setter.call(this, java.lang.Double.parseDouble(opt))

                } else if (java.lang.Boolean.TYPE.isAssignableFrom(returnType)) {
                    m.setter.call(this, java.lang.Boolean.parseBoolean(opt))

                } else if (java.lang.String::class.java.isAssignableFrom(returnType)) {
                    m.setter.call(this, opt)

                } else if (returnType.isEnum) {
                    val valueOfMethod = returnType.getDeclaredMethod("valueOf",
                            java.lang.String::class.java)
                    m.setter.call(this, valueOfMethod.invoke(null, opt))

                } else {
                    throw IllegalStateException("BUG: cannot handle type $returnType")
                }
            } catch (e: Exception) {
                throw IllegalArgumentException("Failed to handle property '${m.name}'", e)
            }
        }
    }

    fun shouldGenerateSqlData() = generateSqlDataWithDSE || generateSqlDataWithSearch

    fun experimentalFeatures(): List<String> {

        val properties = getConfigurationProperties()
                .filter { it.annotations.find { it is Experimental } != null }
                .filter {
                    val returnType = it.returnType.javaType as Class<*>
                    when {
                        java.lang.Boolean.TYPE.isAssignableFrom(returnType) -> it.getter.call(this) as Boolean
                        it.annotations.find { p -> p is Probability && p.activating } != null -> (it.getter.call(this) as Double) > 0
                        else -> false
                    }
                }
                .map { it.name }

        val enums = getConfigurationProperties()
                .filter {
                    val returnType = it.returnType.javaType as Class<*>
                    if (returnType.isEnum) {
                        val e = it.getter.call(this)
                        val f = returnType.getField(e.toString())
                        f.annotations.find { it is Experimental } != null
                    } else {
                        false
                    }
                }
                .map { "${it.name}=${it.getter.call(this)}" }

        return properties.plus(enums)
    }

//------------------------------------------------------------------------
//--- custom annotations

    /**
     * Configuration (CFG in short) for EvoMaster.
     * Properties annotated with [Cfg] can be set from
     * command line.
     * The code in this class uses reflection, on each property
     * marked with this annotation, to build the list of available
     * modifiable configurations.
     */
    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class Cfg(val description: String)

    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class Min(val min: Double)

    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class Max(val max: Double)

    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class Url

    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class Regex(val regex: String)


    /**
     * A double value between 0 and 1
     */
    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class Probability(
            /**
             * Specify if this probability would activate a functionality if greater than 0.
             * If not, it might still not be used, depending on other configurations.
             * This is mainly needed when dealing with @Experimental probabilities that must
             * be put to 0 if they would activate a new feature that is still unstable
             */
            val activating: Boolean = true
    )


    /**
     * This annotation is used to represent properties controlling
     * features that are still work in progress.
     * Do not use them (yet) in production.
     */
    @Target(AnnotationTarget.PROPERTY, AnnotationTarget.FIELD)
    @MustBeDocumented
    annotation class Experimental


    /**
     * This represent one of the main properties to set in EvoMaster.
     * Those are the ones most likely going to be set by practitioners.
     * Note: most of the other properties are mainly for experiments
     */
    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class Important(
            /**
             * The lower value, the more importance.
             * This only impact of options are sorted when displayed
             */
            val priority: Double
    )

    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class Folder

    @Target(AnnotationTarget.PROPERTY)
    @MustBeDocumented
    annotation class FilePath

//------------------------------------------------------------------------
//--- properties

    /*
        WARNING
        if any change is made here, the "options.md" MUST be recreated
        with ConfigToMarkdown

        You will also need to check if any special character you use in the
        descriptions end-up in some screwed-up Markdown layout
     */

    //----- "Important" options, sorted by priority --------------

    val defaultMaxTime = "60s"

    @Important(1.0)
    @Cfg("Maximum amount of time allowed for the search. " +
            " The time is expressed with a string where hours (`h`), minutes (`m`) and" +
            " seconds (`s`) can be specified, e.g., `1h10m120s` and `72m` are both valid" +
            " and equivalent." +
            " Each component (i.e., `h`, `m` and `s`) is optional, but at least one must be specified. " +
            " In other words, if you need to run the search for just `30` seconds, you can write `30s` " +
            " instead of `0h0m30s`." +
            " **The more time is allowed, the better results one can expect**." +
            " But then of course the test generation will take longer." +
            " For how long should _EvoMaster_ be left run?" +
            " The default 1 _minute_ is just for demonstration." +
            " __We recommend to run it between 1 and 24 hours__, depending on the size and complexity " +
            " of the tested application."
    )
    @Regex("(\\s*)((?=(\\S+))(\\d+h)?(\\d+m)?(\\d+s)?)(\\s*)")
    var maxTime = defaultMaxTime

    @Important(2.0)
    @Cfg("The path directory of where the generated test classes should be saved to")
    @Folder
    var outputFolder = "src/em"


    @Important(2.0)
    @Cfg("The name prefix of generated file(s) with the test cases, without file type extension." +
            " In JVM languages, if the name contains '.', folders will be created to represent" +
            " the given package structure." +
            " Also, in JVM languages, should not use '-' in the file name, as not valid symbol" +
            " for class identifiers." +
            " This prefix be combined with the outputFileSuffix to combined the final name." +
            " As EvoMaster can split the generated tests among different files, each will get a label," +
            " and the names will be in the form prefix+label+suffix.")
    @Regex("[-a-zA-Z\$_][-0-9a-zA-Z\$_]*(.[-a-zA-Z\$_][-0-9a-zA-Z\$_]*)*")
    var outputFilePrefix = "EvoMaster"

    @Important(2.0)
    @Cfg("The name suffix for the generated file(s), to be added before the file type extension." +
            " As EvoMaster can split the generated tests among different files, each will get a label," +
            " and the names will be in the form prefix+label+suffix.")
    @Regex("[-a-zA-Z\$_][-0-9a-zA-Z\$_]*(.[-a-zA-Z\$_][-0-9a-zA-Z\$_]*)*")
    var outputFileSuffix = "Test"


    @Deprecated("Should use outputFilePrefix and outputFileSuffix")
    @Cfg("DEPRECATED. Rather use _outputFilePrefix_ and _outputFileSuffix_")
    var testSuiteFileName = ""

    @Important(2.0)
    @Cfg("Specify in which format the tests should be outputted." +
            " If left on `DEFAULT`, then the value specified in the _EvoMaster Driver_ will be used." +
            " But a different value must be chosen if doing Black-Box testing.")
    var outputFormat = OutputFormat.DEFAULT

    @Important(3.0)
    @Cfg("Use EvoMaster in black-box mode. This does not require an EvoMaster Driver up and running. However, you will need to provide further option to specify how to connect to the SUT")
    var blackBox = false

    @Important(3.2)
    @Url
    @Cfg("When in black-box mode for REST APIs, specify where the Swagger schema can be downloaded from")
    var bbSwaggerUrl: String = ""

    @Important(3.5)
    @Url
    @Cfg("When in black-box mode, specify the URL of where the SUT can be reached." +
            " In REST, if this is missing, the URL will be inferred from OpenAPI/Swagger schema." +
            " In GraphQL, this will point to the entry point of the API.")
    var bbTargetUrl: String = ""


    @Important(3.7)
    @Cfg("Rate limiter, of how many actions to do per minute. For example, when making HTTP calls towards" +
            " an external service, might want to limit the number of calls to avoid bombarding such service" +
            " (which could end up becoming equivalent to a DoS attack)." +
            " A value of zero or negative means that no limiter is applied." +
            " This is needed only for black-box testing of remote services.")
    var ratePerMinute = 0

    //-------- other options -------------


    @Cfg("At times, we need to run EvoMaster with printed logs that are deterministic." +
            " For example, this means avoiding printing out time-stamps.")
    var avoidNonDeterministicLogs = false

    enum class Algorithm {
        MIO, RANDOM, WTS, MOSA
    }

    @Cfg("The algorithm used to generate test cases")
    var algorithm = Algorithm.MIO

    /**
    * Workaround for issues with annotations that can not be applied on ENUM values,
    * like @Experimental
    * */
    interface WithExperimentalOptions{
        fun isExperimental() : Boolean
    }

    enum class ProblemType(private val experimental: Boolean) : WithExperimentalOptions {
        DEFAULT(experimental = false),
        REST(experimental = false),
        GRAPHQL(experimental = true),
        WEB(experimental = true);
        override fun isExperimental() = experimental
    }

    @Cfg("The type of SUT we want to generate tests for, e.g., a RESTful API." +
            " If left to DEFAULT, the type will be inferred from the EM Driver." +
            " However, in case of ambiguities (e.g., the driver specifies more than one type)," +
            " then this field must be set with a specific type." +
            " This is also the case for Black-Box testing where there is no EM Driver." +
            " In this latter case, the system defaults to handle REST APIs.")
    var problemType = ProblemType.DEFAULT


    @Cfg("Specify if test classes should be created as output of the tool. " +
            "Usually, you would put it to 'false' only when debugging EvoMaster itself")
    var createTests = true

    enum class TestSuiteSplitType {
        NONE,
        CLUSTER,
        CODE
    }

    @Cfg("Instead of generating a single test file, it could be split in several files, according to different strategies")
    var testSuiteSplitType = TestSuiteSplitType.CLUSTER

    @Cfg("Generate an executive summary, containing an example of each category of potential fault found." +
                    "NOTE: This option is only meaningful when used in conjuction with clustering. " +
                    "This is achieved by turning the option --testSuiteSplitType to CLUSTER")
    var executiveSummary = true

    @Experimental
    @Cfg("The Distance Metric Last Line may use several values for epsilon." +
            "During experimentation, it may be useful to adjust these values. Epsilon describes the size of the neighbourhood used for clustering, so may result in different clustering results." +
            "Epsilon should be between 0.0 and 1.0. If the value is outside of that range, epsilon will use the default of 0.8.")
    @Min(0.0)
    @Max(1.0)
    var lastLineEpsilon = 0.8

    @Experimental
    @Cfg("The Distance Metric Error Text may use several values for epsilon." +
            "During experimentation, it may be useful to adjust these values. Epsilon describes the size of the neighbourhood used for clustering, so may result in different clustering results." +
            "Epsilon should be between 0.0 and 1.0. If the value is outside of that range, epsilon will use the default of 0.8.")
    @Min(0.0)
    @Max(1.0)
    var errorTextEpsilon = 0.8

    @Cfg("The seed for the random generator used during the search. " +
            "A negative value means the CPU clock time will be rather used as seed")
    var seed: Long = -1

    @Cfg("TCP port of where the SUT REST controller is listening on")
    @Min(0.0)
    @Max(65535.0)
    var sutControllerPort = ControllerConstants.DEFAULT_CONTROLLER_PORT

    @Cfg("Host name or IP address of where the SUT REST controller is listening on")
    var sutControllerHost = ControllerConstants.DEFAULT_CONTROLLER_HOST

    @Cfg("Limit of number of individuals per target to keep in the archive")
    @Min(1.0)
    var archiveTargetLimit = 10

    @Cfg("Probability of sampling a new individual at random")
    @Probability
    var probOfRandomSampling = 0.5

    @Cfg("The percentage of passed search before starting a more focused, less exploratory one")
    @PercentageAsProbability(true)
    var focusedSearchActivationTime = 0.5

    @Cfg("Number of applied mutations on sampled individuals, at the start of the search")
    @Min(0.0)
    var startNumberOfMutations = 1

    @Cfg("Number of applied mutations on sampled individuals, by the end of the search")
    @Min(0.0)
    var endNumberOfMutations = 10


    enum class StoppingCriterion {
        TIME,
        FITNESS_EVALUATIONS
    }

    @Cfg("Stopping criterion for the search")
    var stoppingCriterion = StoppingCriterion.TIME


    val defaultMaxActionEvaluations = 1000

    @Cfg("Maximum number of action evaluations for the search." +
            " A fitness evaluation can be composed of 1 or more actions," +
            " like for example REST calls or SQL setups." +
            " The more actions are allowed, the better results one can expect." +
            " But then of course the test generation will take longer." +
            " Only applicable depending on the stopping criterion.")
    @Min(1.0)
    var maxActionEvaluations = defaultMaxActionEvaluations


    val defaultMaxTimeInSeconds = 0

    @Cfg("Maximum number of seconds allowed for the search." +
            " The more time is allowed, the better results one can expect." +
            " But then of course the test generation will take longer." +
            " Only applicable depending on the stopping criterion." +
            " If this value is 0, the setting 'maxTime' will be used instead.")
    @Min(0.0)
    var maxTimeInSeconds = defaultMaxTimeInSeconds


    @Cfg("Whether or not writing statistics of the search process. " +
            "This is only needed when running experiments with different parameter settings")
    var writeStatistics = false

    @Cfg("Where the statistics file (if any) is going to be written (in CSV format)")
    @FilePath
    var statisticsFile = "statistics.csv"

    @Cfg("Whether should add to an existing statistics file, instead of replacing it")
    var appendToStatisticsFile = false

    @Cfg("If positive, check how often, in percentage % of the budget, to collect statistics snapshots." +
            " For example, every 5% of the time.")
    @Max(50.0)
    var snapshotInterval = -1.0

    @Cfg("Where the snapshot file (if any) is going to be written (in CSV format)")
    @FilePath
    var snapshotStatisticsFile = "snapshot.csv"

    @Cfg("An id that will be part as a column of the statistics file (if any is generated)")
    var statisticsColumnId = "-"

    @Cfg("Whether we should collect data on the extra heuristics. Only needed for experiments.")
    var writeExtraHeuristicsFile = false

    @Cfg("Where the extra heuristics file (if any) is going to be written (in CSV format)")
    @FilePath
    var extraHeuristicsFile = "extra_heuristics.csv"


    enum class SecondaryObjectiveStrategy {
        AVG_DISTANCE,
        AVG_DISTANCE_SAME_N_ACTIONS,
        BEST_MIN
    }

    @Cfg("Strategy used to handle the extra heuristics in the secondary objectives")
    var secondaryObjectiveStrategy = SecondaryObjectiveStrategy.AVG_DISTANCE_SAME_N_ACTIONS

    @Cfg("Whether secondary objectives are less important than test bloat control")
    var bloatControlForSecondaryObjective = false

    @Cfg("Specify minimum size when bloatControlForSecondaryObjective")
    @Min(0.0)
    var minimumSizeControl = 2

    @Cfg("Probability of applying a mutation that can change the structure of a test")
    @Probability
    var structureMutationProbability = 0.5


    enum class GeneMutationStrategy {
        ONE_OVER_N,
        ONE_OVER_N_BIASED_SQL
    }

    @Cfg("Strategy used to define the mutation probability")
    var geneMutationStrategy = GeneMutationStrategy.ONE_OVER_N_BIASED_SQL

    enum class FeedbackDirectedSampling {
        NONE,
        LAST,
        FOCUSED_QUICKEST
    }

    @Cfg("Specify whether when we sample from archive we do look at the most promising targets for which we have had a recent improvement")
    var feedbackDirectedSampling = FeedbackDirectedSampling.LAST

    //Warning: this is off in the tests, as it is a source of non-determinism
    @Cfg("Whether to use timestamp info on the execution time of the tests for sampling (e.g., to reward the quickest ones)")
    var useTimeInFeedbackSampling = true


    @Experimental
    @Cfg("When sampling from archive based on targets, decide whether to use weights based on properties of the targets (e.g., a target likely leading to a flag will be sampled less often)")
    var useWeightedSampling = false


    @Cfg("Define the population size in the search algorithms that use populations (e.g., Genetic Algorithms, but not MIO)")
    @Min(1.0)
    var populationSize = 30

    @Cfg("Define the maximum number of tests in a suite in the search algorithms that evolve whole suites, e.g. WTS")
    @Min(1.0)
    var maxSearchSuiteSize = 50

    @Cfg("Probability of applying crossover operation (if any is used in the search algorithm)")
    @Probability
    var xoverProbability = 0.7

    @Cfg("Number of elements to consider in a Tournament Selection (if any is used in the search algorithm)")
    @Min(1.0)
    var tournamentSize = 10

    @Cfg("When sampling new test cases to evaluate, probability of using some smart strategy instead of plain random")
    @Probability
    var probOfSmartSampling = 0.5

    @Cfg("Max number of 'actions' (e.g., RESTful calls or SQL commands) that can be done in a single test")
    @Min(1.0)
    var maxTestSize = 10

    @Cfg("Tracking of SQL commands to improve test generation")
    var heuristicsForSQL = true

    @Cfg("Enable extracting SQL execution info")
    var extractSqlExecutionInfo = true

    @Experimental
    @Cfg("Enable EvoMaster to generate SQL data with direct accesses to the database. Use Dynamic Symbolic Execution")
    var generateSqlDataWithDSE = false

    @Cfg("Enable EvoMaster to generate SQL data with direct accesses to the database. Use a search algorithm")
    var generateSqlDataWithSearch = true

    @Cfg("When generating SQL data, how many new rows (max) to generate for each specific SQL Select")
    @Min(1.0)
    var maxSqlInitActionsPerMissingData = 5


    @Cfg("Maximum size (in bytes) that EM handles response payloads in the HTTP responses. " +
            "If larger than that, a response will not be stored internally in EM during the test generation. " +
            "This is needed to avoid running out of memory.")
    var maxResponseByteSize = 1_000_000

    @Cfg("Whether to print how much search done so far")
    var showProgress = true

    @Experimental
    @Cfg("Whether or not enable a search process monitor for archiving evaluated individuals and Archive regarding an evaluation of search. " +
            "This is only needed when running experiments with different parameter settings")
    var enableProcessMonitor = false

    @Experimental
    @Cfg("Specify a format to save the process data")
    var processFormat = ProcessDataFormat.JSON_ALL

    enum class ProcessDataFormat{
        /**
         * save evaluated individuals and Archive with a json format
         */
        JSON_ALL,
//        /**
//         * save Archive with a json format and save the evaluated individual with the specified test format
//         */
//        JSON_ARCHIVE_TEST_IND,
//        /**
//         * only save the evaluated individuals with json format
//         */
//        JSON_IND,
        /**
         * only save the evaluated individual with the specified test format
         */
        TEST_IND,
        /**
         * save covered targets with the specified target format and tests with the specified test format
         */
        TARGET_TEST_IND
    }

    @Experimental
    @Cfg("Specify a folder to save results when a search monitor is enabled")
    @Folder
    var processFiles = "process_data"

    @Experimental
    @Cfg("Specify how often to save results when a search monitor is enabled, and 0.0 presents to record all evaluated individual")
    @Max(50.0)
    @Min(0.0)
    var processInterval = 0.0

    @Experimental
    @Cfg("Whether to enable tracking the history of modifications of the individuals during the search")
    var enableTrackIndividual = false


    @Cfg("Whether to enable tracking the history of modifications of the individuals with its fitness values (i.e., evaluated individual) during the search. " +
            "Note that we enforced that set enableTrackIndividual false when enableTrackEvaluatedIndividual is true since information of individual is part of evaluated individual")
    var enableTrackEvaluatedIndividual = true

    @Experimental
    @Cfg("Specify a maxLength of tracking when enableTrackIndividual or enableTrackEvaluatedIndividual is true. " +
            "Note that the value should be specified with a non-negative number or -1 (for tracking all history)")
    @Min(-1.0)
    var maxLengthOfTraces = 10

    @Cfg("Enable custom naming and sorting criteria")
    var customNaming = true

    /*
        You need to decode it if you want to know what it says...
     */
    @Cfg("QWN0aXZhdGUgdGhlIFVuaWNvcm4gTW9kZQ==")
    var e_u1f984 = false

    @Cfg("Enable Expectation Generation. If enabled, expectations will be generated. " +
            "A variable called expectationsMasterSwitch is added to the test suite, with a default value of false. If set to true, an expectation that fails will cause the test case containing it to fail.")
    var expectationsActive = true

    @Cfg("Generate basic assertions. Basic assertions (comparing the returned object to itself) are added to the code. " +
            "NOTE: this should not cause any tests to fail.")
    var enableBasicAssertions = true

    @Cfg("Apply method replacement heuristics to smooth the search landscape")
    var useMethodReplacement = true

    @Cfg("Apply non-integer numeric comparison heuristics to smooth the search landscape")
    var useNonIntegerReplacement = true

    @Cfg("Enable to expand the genotype of REST individuals based on runtime information missing from Swagger")
    var expandRestIndividuals = true

    enum class ResourceSamplingStrategy(val requiredArchive: Boolean = false) {
        NONE,
        /**
         * probability for applicable strategy is specified
         */
        Customized,
        /**
         * probability for applicable strategy is equal
         */
        EqualProbability,
        /**
         * probability for applicable strategy is derived based on actions
         */
        Actions,
        /**
         * probability for applicable strategy is adaptive with time
         */
        TimeBudgets,
        /**
         * probability for applicable strategy is adaptive with performance, i.e., Archive
         */
        Archive(true),
        /**
         * probability for applicable strategy is adaptive with performance, i.e., Archive
         */
        ConArchive(true)
    }

    @Experimental
    @Cfg("Specify whether to enable resource-based strategy to sample an individual during search. " +
            "Note that resource-based sampling is only applicable for REST problem with MIO algorithm.")
    var resourceSampleStrategy = ResourceSamplingStrategy.NONE

    @Experimental
    @Cfg("Specify whether to enable resource dependency heuristics, i.e, probOfEnablingResourceDependencyHeuristics > 0.0. " +
            "Note that the option is available to be enabled only if resource-based smart sampling is enable. " +
            "This option has an effect on sampling multiple resources and mutating a structure of an individual.")
    @Probability
    var probOfEnablingResourceDependencyHeuristics = 0.0

    @Experimental
    @Cfg("Specify whether to export derived dependencies among resources")
    var exportDependencies = false

    @Experimental
    @Cfg("Specify a file that saves derived dependencies")
    @FilePath
    var dependencyFile = "dependencies.csv"

    @Experimental
    @Cfg("Specify a probability to apply SQL actions for preparing resources for REST Action")
    @Probability
    var probOfApplySQLActionToCreateResources = 0.0

    @Experimental
    @Cfg("When generating resource using SQL (e.g., sampler or mutator), how many new rows (max) to generate for the specific resource each time")
    @Min(0.0)
    var maxSqlInitActionsPerResource = 0

    @Experimental
    @Cfg("Specify a strategy to determinate a number of resources to be manipulated throughout the search.")
    var employSqlNumResourceStrategy = SqlInitResourceStrategy.NONE

    enum class SqlInitResourceStrategy{
        NONE,

        /**
         * determinate a number of resource to be manipulated at random between 1 and [maxSqlInitActionsPerResource]
         */
        RANDOM,
        /**
         * adaptively decrease a number of resources to be manipulated from [maxSqlInitActionsPerResource] to 1
         */
        DPC
    }
    @Experimental
    @Cfg("Specify a minimal number of rows in a table that enables selection (i.e., SELECT sql) to prepare resources for REST Action. " +
            "In other word, if the number is less than the specified, insertion is always applied.")
    @Min(0.0)
    var minRowOfTable = 10

    @Experimental
    @Cfg("Specify a probability that enables selection (i.e., SELECT sql) of data from database instead of insertion (i.e., INSERT sql) for preparing resources for REST actions")
    @Probability(false)
    var probOfSelectFromDatabase = 0.1

    @Experimental
    @Cfg("Whether to apply text/name analysis with natural language parser to derive relationships between name entities, e.g., a resource identifier with a name of table")
    var doesApplyNameMatching = false

    @Experimental
    @Cfg("Whether to employ NLP parser to process text. " +
            "Note that to enable this parser, it is required to build the EvoMaster with the resource profile, i.e., mvn clean install -Presourceexp -DskipTests")
    var enableNLPParser = false

    @Experimental
    @Cfg("Whether to save mutated gene info, which is typically used for debugging mutation")
    var saveMutationInfo = false

    @Experimental
    @Cfg("Specify a path to save mutation details which is useful for debugging mutation")
    @FilePath
    var mutatedGeneFile = "mutatedGeneInfo.csv"

    @Experimental
    @Cfg("Specify a strategy to select targets for evaluating mutation")
    var mutationTargetsSelectionStrategy = MutationTargetsSelectionStrategy.FIRST_NOT_COVERED_TARGET

    enum class MutationTargetsSelectionStrategy{
        /**
         * employ not covered target obtained by archive at first for all upTimesMutations
         *
         * e.g., mutate an individual with 10times, at first, the current not covered target is {A, B}
         * after the 2nd mutation, A is covered, C is newly reached,
         * for next mutation, that target employed for the comparison is still {A, B}
         */
        FIRST_NOT_COVERED_TARGET,
        /**
         * expand targets with updated not covered targets
         *
         * e.g., mutate an individual with 10times, at first, the current not covered target is {A, B}
         * after the 2nd mutation, A is covered, C is newly reached,
         * for next mutation, that target employed for the comparison is {A, B, C}
         */
        EXPANDED_UPDATED_NOT_COVERED_TARGET,
        /**
         * only employ current not covered targets obtainedby archive
         *
         * e.g., mutate an individual with 10times, at first, the current not covered target is {A, B}
         * after the 2nd mutation, A is covered, C is newly reached,
         * for next mutation, that target employed for the comparison is {B, C}
         */
        UPDATED_NOT_COVERED_TARGET
    }

    @Experimental
    @Cfg("Whether to record targets when the number is more than 100")
    var recordExceededTargets = false

    @Experimental
    @Cfg("Specify a path to save all not covered targets when the number is more than 100")
    @FilePath
    var exceedTargetsFile = "exceedTargets.txt"

    @Experimental
    @Cfg("Specify a probability to apply S1iR when resource sampling strategy is 'Customized'")
    @Probability(false)
    var S1iR: Double = 0.25

    @Experimental
    @Cfg("Specify a probability to apply S1dR when resource sampling strategy is 'Customized'")
    @Probability(false)
    var S1dR: Double = 0.25

    @Experimental
    @Cfg("Specify a probability to apply S2dR when resource sampling strategy is 'Customized'")
    @Probability(false)
    var S2dR: Double = 0.25

    @Experimental
    @Cfg("Specify a probability to apply SMdR when resource sampling strategy is 'Customized'")
    @Probability(false)
    var SMdR: Double = 0.25

    @Cfg("Whether to enable a weight-based mutation rate")
    var weightBasedMutationRate = true

    @Cfg("Whether to specialize sql gene selection to mutation")
    var specializeSQLGeneSelection = true

    @Experimental
    @Cfg("Specify a maximum mutation rate when enabling 'adaptiveMutationRate'")
    @PercentageAsProbability(false)
    var maxMutationRate = 0.9

    @Experimental
    @Cfg("Specify a starting percentage of genes of an individual to mutate")
    @PercentageAsProbability(false)
    var startingPerOfGenesToMutate = 0.5

    @Cfg("When weight-based mutation rate is enabled, specify a percentage of calculating mutation rate based on a number of candidate genes to mutate. " +
            "For instance, d = 1.0 means that the mutation rate fully depends on a number of candidate genes to mutate, " +
            "and d = 0.0 means that the mutation rate fully depends on weights of candidates genes to mutate.")
    @PercentageAsProbability(false)
    var d = 0.8

    @Cfg("Specify a probability to enable archive-based mutation")
    @Probability
    var probOfArchiveMutation = 0.5

    @Experimental
    @Cfg("Specify whether to collect impact info that provides an option to enable of collecting impact info when archive-based gene selection is disable. ")
    var doCollectImpact = false

    @Experimental
    @Cfg("During mutation, whether to abstract genes for repeated SQL actions")
    var abstractInitializationGeneToMutate = false

    @Cfg("Specify a strategy to calculate a weight of a gene based on impacts")
    var geneWeightBasedOnImpactsBy = GeneWeightBasedOnImpact.RATIO

    enum class GeneWeightBasedOnImpact{
        /**
         * using rank of counter
         */
        SORT_COUNTER,
        /**
         * using rank of ratio
         */
        SORT_RATIO,
        /**
         * using counter
         */
        COUNTER,
        /**
         * using ratio, ie, counter/total manipulated times
         */
        RATIO
    }

    @Cfg("Specify a strategy to select genes for mutation adaptively")
    var adaptiveGeneSelectionMethod = GeneMutationSelectionMethod.APPROACH_IMPACT

    @Cfg("Specify whether to enable weight-based mutation selection for selecting genes to mutate for a gene")
    var enableWeightBasedMutationRateSelectionForGene = true

    @Experimental
    @Cfg("Whether to save archive info after each of mutation, which is typically useful for debugging mutation and archive")
    var saveArchiveAfterMutation = false

    @Experimental
    @Cfg("Specify a path to save archive after each mutation during search, only useful for debugging")
    @FilePath
    var archiveAfterMutationFile = "archive.csv"

    @Experimental
    @Cfg("Whether to save impact info after each of mutation, which is typically useful debugging impact driven solutions and mutation")
    var saveImpactAfterMutation = false

    @Experimental
    @Cfg("Specify a path to save collected impact info after each mutation during search, only useful for debugging")
    @FilePath
    var impactAfterMutationFile = "impactSnapshot.csv"

    @Cfg("Whether to enable archive-based gene mutation")
    var archiveGeneMutation = ArchiveGeneMutation.SPECIFIED_WITH_SPECIFIC_TARGETS

    @Cfg("Specify a maximum length of history when applying archive-based gene mutation")
    var maxlengthOfHistoryForAGM = 10

    /**
     * archive-based gene value mutation
     */
    enum class ArchiveGeneMutation (val withTargets : Int = 0, val withDirection: Boolean = false){
        /**
         * do not apply archive-based gene mutation
         */
        NONE,
        /**
         * mutate with history but not related to any target
         */
        SPECIFIED,
        /**
         * mutate individual with history based on targets
         * but not specific to actions
         */
        SPECIFIED_WITH_TARGETS(1, false),
        /**
         * mutate individual with history based on targets
         * and the targets are linked to the action level
         */
        SPECIFIED_WITH_SPECIFIC_TARGETS(2, false),
        /**
         * mutate individual with history and directions based on targets
         * but not specific to actions
         */
        SPECIFIED_WITH_TARGETS_DIRECTION(1, true),
        /**
         * mutate individual with history and directions based on targets
         * and the targets are linked to the action level
         */
        SPECIFIED_WITH_SPECIFIC_TARGETS_DIRECTION(2, true),

        /**
         * mutate individual with history with consideration of dependency among genes
         * (not done yet)
         */
        ADAPTIVE
    }

    @Experimental
    @Cfg("Specify whether to export derived impacts among genes")
    var exportImpacts = false

    @Experimental
    @Cfg("Specify a path to save derived genes")
    @FilePath
    var impactFile = "impact.csv"

    @Experimental
    @Cfg("Specify whether to disable structure mutation during focus search")
    var disableStructureMutationDuringFocusSearch = false

    @Cfg("Probability to use input tracking (i.e., a simple base form of taint-analysis) to determine how inputs are used in the SUT")
    @Probability
    var baseTaintAnalysisProbability = 0.9

    @Cfg("Only used when running experiments for black-box mode, where an EvoMaster Driver would be present, and can reset state after each experiment")
    var bbExperiments = false

    @Cfg("Specify whether to export covered targets info")
    var exportCoveredTarget = false

    @Cfg("Specify a file which saves covered targets info regarding generated test suite")
    @FilePath
    var coveredTargetFile = "coveredTargets.txt"

    @Experimental
    @Cfg("Specify a format to organize the covered targets by the search")
    var coveredTargetSortedBy = SortCoveredTargetBy.NAME


    @FilePath
    @Cfg("When generating tests in JavaScript, there is the need to know where the driver is located in respect to" +
            " the generated tests")
    var jsControllerPath = "./app-driver.js"

    enum class SortCoveredTargetBy {
        /**
         * sorted by ids of targets alphabetically
         */
        NAME,
        /**
         * grouped by tests and sorted by index of tests.
         * it may help to analyze the individuals regarding different strategies.
         */
        TEST
        /**
         * there might be other options, e.g., based on class,
         * but we need to follow rules to encode and decode regarding id.
         */
    }

    @Cfg("Only for debugging. Concentrate search on only one single REST endpoint")
    var endpointFocus : String? = null

    @Experimental
    @Cfg("Whether to seed EvoMaster with some initial test cases. These test cases will be used and evolved throughout the search process")
    var seedTestCases = false

    enum class SeedTestCasesFormat {
        POSTMAN
    }

    @Experimental
    @Cfg("Format of the test cases seeded to EvoMaster")
    var seedTestCasesFormat = SeedTestCasesFormat.POSTMAN

    @Experimental
    @FilePath
    @Cfg("File path where the seeded test cases are located")
    var seedTestCasesPath : String = "postman.postman_collection.json"

    @Cfg("Try to enforce the stopping of SUT business-level code." +
            " This is needed when TCP connections timeouts, to avoid thread executions" +
            " from previous HTTP calls affecting the current one")
    var killSwitch = true


    @Experimental
    @Cfg("Whether to skip failed SQL commands in the generated test files")
    var skipFailureSQLInTestFile = false


    fun timeLimitInSeconds(): Int {
        if (maxTimeInSeconds > 0) {
            return maxTimeInSeconds
        }

        val h = maxTime.indexOf('h')
        val m = maxTime.indexOf('m')
        val s = maxTime.indexOf('s')

        val hours = if (h >= 0) {
            maxTime.subSequence(0, h).toString().trim().toInt()
        } else 0

        val minutes = if (m >= 0) {
            maxTime.subSequence(if (h >= 0) h + 1 else 0, m).toString().trim().toInt()
        } else 0

        val seconds = if (s >= 0) {
            maxTime.subSequence(if (m >= 0) m + 1 else (if (h >= 0) h + 1 else 0), s).toString().trim().toInt()
        } else 0

        return (hours * 60 * 60) + (minutes * 60) + seconds
    }

    fun trackingEnabled() = enableTrackEvaluatedIndividual || enableTrackIndividual

    /**
     * impact info can be collected when archive-based solution is enabled or doCollectImpact
     */
    fun isEnabledImpactCollection() = algorithm == Algorithm.MIO && doCollectImpact || isEnabledArchiveGeneSelection()

    /**
     * @return whether archive-based gene selection is enabled
     */
    fun isEnabledArchiveGeneSelection() = algorithm == Algorithm.MIO && probOfArchiveMutation > 0.0 && adaptiveGeneSelectionMethod != GeneMutationSelectionMethod.NONE

    /**
     * @return whether archive-based gene mutation is enabled based on the configuration, ie, EMConfig
     */
    fun isEnabledArchiveGeneMutation() = algorithm == Algorithm.MIO && archiveGeneMutation != ArchiveGeneMutation.NONE && probOfArchiveMutation > 0.0

    fun isEnabledArchiveSolution() = isEnabledArchiveGeneMutation() || isEnabledArchiveGeneSelection()

    /**
     * @return whether enable resource-dependency based method
     */
    fun isEnabledResourceDependency() = probOfSmartSampling > 0.0 && resourceSampleStrategy != ResourceSamplingStrategy.NONE

    /**
     * @return whether to generate SQL between rest actions
     */
    fun isEnabledSQLInBetween() = isEnabledResourceDependency() && heuristicsForSQL && probOfApplySQLActionToCreateResources > 0.0

}