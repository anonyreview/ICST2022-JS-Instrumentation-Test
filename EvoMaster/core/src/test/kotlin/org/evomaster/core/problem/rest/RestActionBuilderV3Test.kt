package org.evomaster.core.problem.rest

import io.swagger.parser.OpenAPIParser
import org.evomaster.core.problem.rest.param.BodyParam
import org.evomaster.core.problem.rest.param.FormParam
import org.evomaster.core.search.Action
import org.evomaster.core.search.gene.ArrayGene
import org.evomaster.core.search.gene.ObjectGene
import org.evomaster.core.search.gene.OptionalGene
import org.evomaster.core.search.gene.StringGene
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

class RestActionBuilderV3Test{

    @Test
    fun testParseDto(){

        val name = "com.FooBar"
        val foo = "foo"
        val bar = "bar"

        val dtoSchema = """
            "$name": {
                 "type": "object",
                 "properties": {
                        "$foo": { 
                            "type": "string"
                        },
                        "$bar": {
                            "type": "integer"
                        }
                 },
                 "required": [
                    "$foo"
                 ]
            }     
        """.trimIndent()

        val gene = RestActionBuilderV3.createObjectGeneForDTO(name, dtoSchema, name) as ObjectGene
        assertEquals(name, gene.name)
        assertEquals(2, gene.fields.size)

        val str = gene.fields.find { it is StringGene } as StringGene
        assertEquals(foo, str.name)

        val nr = gene.fields.find { it is OptionalGene } as OptionalGene
        assertEquals(bar, nr.name)
    }




    //---------------------------------

    private fun loadAndAssertActions(resourcePath: String, expectedNumberOfActions: Int)
            : MutableMap<String, Action> {


        val schema = OpenAPIParser().readLocation(resourcePath, null, null).openAPI

        val actions: MutableMap<String, Action> = mutableMapOf()

        RestActionBuilderV3.addActionsFromSwagger(schema, actions)

        assertEquals(expectedNumberOfActions, actions.size)

        return actions
    }

    // ----------- V3 --------------

    @Test
    fun testNexmo(){
        loadAndAssertActions("/swagger/apisguru-v3/nexmo.json", 5)
    }

    @Test
    fun testBcgnews() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/bcgnws.json", 14)
    }

    @Test
    fun testBclaws() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/bclaws.json", 7)
    }

    @Test
    fun testBng2latlong() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/bng2latlong.json", 1)
    }

    @Test
    fun testChecker() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/checker.json", 1)
    }

    @Test
    fun testDisposable() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/disposable.json", 1)
    }

    @Test
    fun testFraudDetection() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/fraud-detection.json", 2)
    }

    @Test
    fun testGeolocation() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/geolocation.json", 1)
    }

    @Test
    fun testIp2proxy() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/ip2proxy.com.json", 1)
    }

    @Test
    fun testApisGuruNews() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/news.json", 27)
    }

    @Test
    fun testOpen511() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/open511.json", 4)
    }

    @Test
    fun testSmsVerification() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/sms-verification.json", 2)
    }

    @Test
    fun testValidation() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/validation.json", 1)
    }



    // ----------- V2 --------------

    @Test
    fun testGitLab() {
        loadAndAssertActions("/swagger/others/gitlab.json", 358)
    }

    @Test
    fun testCyclotron() {
        loadAndAssertActions("/swagger/others/cyclotron.json", 50)
    }


    @Test
    fun testPetStore() {
        loadAndAssertActions("/swagger/others/petstore.json", 20)
    }


    @Test
    fun testMultiParamPath() {
        loadAndAssertActions("/swagger/artificial/multi_param_path.json", 1)
    }


    @Test
    fun testNews() {
        val map = loadAndAssertActions("/swagger/sut/news.json", 7)

        val create = map["POST:/news"] as RestCallAction
        assertEquals(2, create.seeGenes().size)
        val bodyNews = create.seeGenes().find { it.name == "body" }
        assertNotNull(bodyNews)
        assertNotNull(bodyNews is OptionalGene)
        assertNotNull((bodyNews as OptionalGene).gene is ObjectGene)
        assertNotNull((bodyNews.gene as ObjectGene).refType)
        assertEquals("NewsDto", (bodyNews.gene as ObjectGene).refType)

    }


    @Test
    fun testCatWatch() {
        val map = loadAndAssertActions("/swagger/sut/catwatch.json", 23)

        val postScoring = map["POST:/config/scoring.project"] as RestCallAction
        assertEquals(3, postScoring.seeGenes().size)
        val bodyPostScoring = postScoring.seeGenes().find { it.name == "body" }
        assertNotNull(bodyPostScoring)
        assertTrue(bodyPostScoring is OptionalGene)
        assertTrue((bodyPostScoring as OptionalGene).gene is StringGene)
    }

    @Test
    fun testProxyPrint() {

        //TODO check  Map<String, String> in additionalProperties

        val map = loadAndAssertActions("/swagger/sut/proxyprint.json", 115)

        val balance = map["GET:/consumer/balance"] as RestCallAction
        //Principal should not appear, because anyway it is a GET
        assertTrue(balance.parameters.none { it is BodyParam })


        val update = map["PUT:/consumer/info/update"] as RestCallAction
        //   Type is JSON, but no body info besides wrong Principal
        assertTrue(update.parameters.none { it is BodyParam })


        val register = map["POST:/consumer/register"] as RestCallAction
        // Same for WebRequest
        assertTrue(register.parameters.none { it is BodyParam })

    }

    @Test
    fun testCreateActions() {
        loadAndAssertActions("/swagger/artificial/positive_integer_swagger.json", 2)
    }


    @Test
    fun testOCVN() {
        loadAndAssertActions("/swagger/sut/ocvn_1oc.json", 192)
    }

    @Disabled("This is a bug in Swagger Core, reported at https://github.com/swagger-api/swagger-core/issues/2100")
    @Test
    fun testFeaturesServicesNull() {
        loadAndAssertActions("/swagger/sut/features_service_null.json", 18)
    }

    @Test
    fun testFeaturesServices() {
        loadAndAssertActions("/swagger/sut/features_service.json", 18)
    }

    @Test
    fun testScoutApi() {
        loadAndAssertActions("/swagger/sut/scout-api.json", 49)
    }


    @Test
    fun testBranches() {
        loadAndAssertActions("/swagger/artificial/branches.json", 3)
    }



    //TODO need to handle "multipart/form-data"
    @Disabled
    @Test
    fun testSimpleForm() {
        val actions = loadAndAssertActions("/swagger/artificial/simpleform.json", 1)

        val a = actions.values.first() as RestCallAction

        assertEquals(HttpVerb.POST, a.verb)
        assertEquals(2, a.parameters.size)
        assertEquals(2, a.parameters.filter { p -> p is FormParam }.size)
    }

    @Test
    fun testDuplicatedParamsInFeaturesServices() {
        val actions = loadAndAssertActions("/swagger/sut/features_service.json", 18)
        (actions["POST:/products/{productName}/configurations/{configurationName}/features/{featureName}"] as RestCallAction).apply {
            assertEquals(3, parameters.size)
        }
    }


    @Test
    fun testApisGuru() {

        val actions = loadAndAssertActions("/swagger/apisguru-v2/apis.guru.json", 2)

        actions.values
                .filterIsInstance<RestCallAction>()
                .forEach {
                    assertEquals(2, it.produces.size)
                    assertTrue(it.produces.any{ p -> p.contains("application/json")})
                }
    }

    @Test
    fun testGreenPeace() {
        loadAndAssertActions("/swagger/apisguru-v2/greenpeace.org.json", 6)
    }

    @ParameterizedTest
    @ValueSource(strings = ["/swagger/artificial/reference_type_v2.json","/swagger/artificial/reference_type_v3.json"])
    fun testReferenceType(path: String){
        val actions = loadAndAssertActions(path, 1)
        val bodyParam = actions.values.filterIsInstance<RestCallAction>().flatMap { it.parameters }.filterIsInstance<BodyParam>()
        assertEquals(1, bodyParam.size)
        assertTrue(bodyParam.first().gene is ObjectGene)
        (bodyParam.first().gene as ObjectGene).apply {
            assertNotNull(refType)
            assertEquals("Component", refType)
            val info = (fields.find { it.name == "info" } as? OptionalGene)
            assertNotNull(info)
            assertTrue((info as OptionalGene).gene is ObjectGene)
            (info.gene as ObjectGene).apply {
                assertNotNull(refType)
                assertEquals("Info", refType)
                val at = (fields.find { it.name =="at" } as? OptionalGene)
                assertNotNull(at)
                assertTrue((at as OptionalGene).gene is ArrayGene<*>)
                (at.gene as ArrayGene<*>).apply {
                    assertTrue(template is ObjectGene)
                    assertEquals("AT", (template as ObjectGene).refType)
                }
            }

        }
    }
}