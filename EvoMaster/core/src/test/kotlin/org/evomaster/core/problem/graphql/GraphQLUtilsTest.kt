package org.evomaster.core.problem.graphql

import com.google.gson.Gson
import org.evomaster.core.problem.graphql.schema.SchemaObj
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import java.io.File
import java.util.*

class GraphQLUtilsTest {

    @Test
    fun graphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/MelodyRepo2.json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(4, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(2, GraphQLUtils.getNumberOfQueriesOrMutations("query", queryGraph))
        assertEquals(3, GraphQLUtils.getNumberOfEdges(queryGraph))
        assertEquals(listOf(2, 1, 1), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Package", "Version"), GraphQLUtils.longestPath(paths))
        assertEquals(0, GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.UNION_TAG, queryGraph))
    }

    @Test
    fun petClinicFragmentGraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/PetsClinic(Fragment).json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(5, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(7, 5, 0, 0), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Owner", "Pet", "PetType"), GraphQLUtils.longestPath(paths))
    }


    @Test
    fun petClinicFragment2GraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/PetsClinic(Fragment2).json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(5, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(7, 6, 0, 0), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Owner", "Pet", "PetType"), GraphQLUtils.longestPath(paths))

    }


    @Test
    fun cyclesEgGraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/CyclesEg.json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(3, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(4, 2), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Job", "Commitment"), GraphQLUtils.longestPath(paths))

    }


    @Test
    fun universeCyclesFragmentGraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/UniverseCycles(Fragment).json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val mutationGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "mutation", " ", mutationGraph, mutableListOf(), mutableSetOf())
        assertEquals(4, GraphQLUtils.getGraphSize(mutationGraph))
        assertEquals(1, GraphQLUtils.getNumberOfQueriesOrMutations("mutation", mutationGraph))
        assertEquals(listOf(1, 1, 1), GraphQLUtils.getNumberOfFields(mutationGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "mutation", mutationGraph, paths)
        assertEquals(listOf("mutation", "AddOnCreatePayload", "AddOn", "AddOnRate"), GraphQLUtils.longestPath(paths))

    }


    @Test
    fun unionInternalEgFragmentGraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/unionInternalEg(Fragment).json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(5, GraphQLUtils.getGraphSize(queryGraph))
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(listOf(1, 1, 1, 1), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Store", "Bouquet", "Flower"), GraphQLUtils.longestPath(paths))
        assertEquals(1, GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.UNION_TAG, queryGraph))

    }

    @Test
    fun unionInternalRecEgFragmentGraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/unionInternalRecEg(Fragment).json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(5, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(1, 1, 2, 1), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Store", "Bouquet", "Flower"), GraphQLUtils.longestPath(paths))
        assertEquals(1, GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.UNION_TAG, queryGraph))
    }

    @Test
    fun unionInternalRecEg2GraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/unionInternalRecEg2.json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(6, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(1, 1, 1, 2, 1), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "VulnerabilityConnection", "Vulnerability", "VulnerabilityDetail", "VulnerabilityDetailList"), GraphQLUtils.longestPath(paths))
        assertEquals(1, GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.UNION_TAG, queryGraph))
    }

    @Test
    fun interfaceInternalEgGraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/interfaceInternalEg.json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(5, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(1, 3, 2, 3), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Store1", "Store", "FlowerStore"), GraphQLUtils.longestPath(paths))
        assertEquals(1, GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.INTERFACE_TAG, queryGraph))
    }


    @Test
    fun interfaceEgGraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/interfaceEg.json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(4, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(3, 2, 3), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Store", "FlowerStore"), GraphQLUtils.longestPath(paths))
        assertEquals(1, GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.INTERFACE_TAG, queryGraph))
    }


    @Test
    fun interfaceHisGraphTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/interfaceHis.json").readText()

        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(16, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(2, 1, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), GraphQLUtils.getNumberOfFields(queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        assertEquals(listOf("query", "Node", "Agency", "Route", "Pattern", "Trip"), GraphQLUtils.longestPath(paths))
        assertEquals(1, GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.INTERFACE_TAG, queryGraph))
    }


    @Test
    fun allNodesReachableTest() {

        val json = GraphQLUtilsTest::class.java.getResource("/graphql/abstract.json").readText()
        val state = GraphQLActionBuilder.TempState()
        val gson = Gson()
        val schemaObj: SchemaObj = gson.fromJson(json, SchemaObj::class.java)
        GraphQLActionBuilder.initTablesInfo(schemaObj, state)
        val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
        GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
        assertEquals(7, GraphQLUtils.getGraphSize(queryGraph))
        assertEquals(listOf(3, 1, 1, 2, 1, 1), GraphQLUtils.getNumberOfFields(queryGraph))
        assertEquals(setOf("E"), GraphQLUtils.getAdjacent("D", queryGraph))
        /**/
        val visitedVertex: MutableSet<String> = mutableSetOf()
        val stack: Deque<String> = ArrayDeque<String>()
        val paths: MutableList<List<String>> = mutableListOf()
        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
        /**/
        assertEquals(listOf(listOf("A", "B", "C", "D", "E"), listOf("A", "F", "E")), GraphQLUtils.getAllPaths("A", "E", queryGraph))
        assertEquals(listOf(listOf("query", "A", "B", "C", "D", "E"), listOf("query", "A", "F", "E"), listOf("query", "B", "C", "D", "E")), GraphQLUtils.getAllPaths("query", "E", queryGraph))
        /**/
        assertEquals(listOf("query", "A", "F", "E"), GraphQLUtils.shortestPath(GraphQLUtils.getAllPaths("query", "E", queryGraph)))
        /**/
        val shortestPFromEachEP = GraphQLUtils.getShortestPathFromEachEntryPointToEachNode(queryGraph)
        assertEquals(listOf(listOf("A", "B"),
                listOf("A", "B", "C"),
                listOf("A", "B", "C", "D"),
                listOf("A", "F", "E"),
                listOf("A", "F"),
                listOf(),
                listOf("B", "C"),
                listOf("B", "C", "D"),
                listOf("B", "C", "D", "E"),
                listOf()), shortestPFromEachEP)

        val minPAmongAllEPForEachNode = GraphQLUtils.minPathAmongAllEntryPointsForEachNode(shortestPFromEachEP)

        assertEquals(listOf(listOf("B", "C"),
                listOf("B", "C", "D"),
                listOf("A", "F"),
                listOf("A", "F", "E")), minPAmongAllEPForEachNode)

        val maxPathAmongAllEPForAllNodes = GraphQLUtils.maxPathAmongAllEntryPointsForAllNodes(minPAmongAllEPForEachNode)

        assertEquals(GraphQLUtils.PathInfo(size = 3, path = listOf("B", "C", "D")), maxPathAmongAllEPForAllNodes)
    }


    @Test
    fun getStatsFromSchemas() {

        val jsonFiles: MutableMap<String, String> = mutableMapOf()
        val buffer = StringBuffer()

        jsonFiles["PetsClinic"] = GraphQLUtilsTest::class.java.getResource("/graphql/PetsClinic.json").readText()
        jsonFiles["AniList"] = GraphQLUtilsTest::class.java.getResource("/graphql/AniList.json").readText()
        jsonFiles["Bitquery"] = GraphQLUtilsTest::class.java.getResource("/graphql/Bitquery.json").readText()
        // jsonFiles["GitLab"] = GraphQLUtilsTest::class.java.getResource("/graphql/GitLab.json").readText()//Todo not working
        jsonFiles["DigitransitHSL"] = GraphQLUtilsTest::class.java.getResource("/graphql/DigitransitHSL.json").readText()
        jsonFiles["TravelgateX"] = GraphQLUtilsTest::class.java.getResource("/graphql/TravelgateX.json").readText()
        jsonFiles["Universe"] = GraphQLUtilsTest::class.java.getResource("/graphql/Universe.json").readText()
        jsonFiles["CatalysisHub"] = GraphQLUtilsTest::class.java.getResource("/graphql/CatalysisHub.json").readText()
        jsonFiles["Contentful"] = GraphQLUtilsTest::class.java.getResource("/graphql/Contentful.json").readText()
        jsonFiles["Countries"] = GraphQLUtilsTest::class.java.getResource("/graphql/Countries.json").readText()
        jsonFiles["DeutscheBahn"] = GraphQLUtilsTest::class.java.getResource("/graphql/DeutscheBahn.json").readText()
        jsonFiles["EHRI"] = GraphQLUtilsTest::class.java.getResource("/graphql/EHRI.json").readText()
        jsonFiles["EtMDB"] = GraphQLUtilsTest::class.java.getResource("/graphql/EtMDB.json").readText()
        jsonFiles["Everbase"] = GraphQLUtilsTest::class.java.getResource("/graphql/Everbase.json").readText()
        jsonFiles["GraphQLJobs"] = GraphQLUtilsTest::class.java.getResource("/graphql/GraphQLJobs.json").readText()
        jsonFiles["HIVDB"] = GraphQLUtilsTest::class.java.getResource("/graphql/HIVDB.json").readText()
        jsonFiles["MelodyRepo"] = GraphQLUtilsTest::class.java.getResource("/graphql/MelodyRepo.json").readText()
        jsonFiles["MelodyRepo2"] = GraphQLUtilsTest::class.java.getResource("/graphql/MelodyRepo2.json").readText()
        jsonFiles["ReactFinland"] = GraphQLUtilsTest::class.java.getResource("/graphql/ReactFinland.json").readText()
        jsonFiles["recEg"] = GraphQLUtilsTest::class.java.getResource("/graphql/recEg.json").readText()
        jsonFiles["SpaceX"] = GraphQLUtilsTest::class.java.getResource("/graphql/SpaceX.json").readText()
        jsonFiles["Book"] = GraphQLUtilsTest::class.java.getResource("/graphql/Book.json").readText()
        jsonFiles["interfaceEg"] = GraphQLUtilsTest::class.java.getResource("/graphql/interfaceEg.json").readText()
        jsonFiles["interfaceInternalEg"] = GraphQLUtilsTest::class.java.getResource("/graphql/interfaceInternalEg.json").readText()
        jsonFiles["unionInternalEg"] = GraphQLUtilsTest::class.java.getResource("/graphql/unionInternalEg.json").readText()
        jsonFiles["unionInternalRecEg"] = GraphQLUtilsTest::class.java.getResource("/graphql/unionInternalRecEg.json").readText()
        jsonFiles["unionInternalRecEg2"] = GraphQLUtilsTest::class.java.getResource("/graphql/unionInternalRecEg2.json").readText()
        jsonFiles["enumInterface"] = GraphQLUtilsTest::class.java.getResource("/graphql/enumInterface.json").readText()
        jsonFiles["interfaceHis"] = GraphQLUtilsTest::class.java.getResource("/graphql/interfaceHis.json").readText()
        jsonFiles["recEg2"] = GraphQLUtilsTest::class.java.getResource("/graphql/recEg2.json").readText()

        jsonFiles.forEach {
            val state = GraphQLActionBuilder.TempState()
            val gson = Gson()
            val schemaObj: SchemaObj = gson.fromJson(it.value, SchemaObj::class.java)
            GraphQLActionBuilder.initTablesInfo(schemaObj, state)

            var visitedVertex: MutableSet<String> = mutableSetOf()
            var stack: Deque<String> = ArrayDeque<String>()
            var paths: MutableList<List<String>> = mutableListOf()
            val queryGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()

            GraphQLUtils.constructGraph(state, "query", " ", queryGraph, mutableListOf(), mutableSetOf())
            buffer.append("************* ${it.key}:").append(System.getProperty("line.separator"))


            if (GraphQLUtils.getGraphSize(queryGraph) != 0) {
                buffer.append("The number of fields in the type Query: ${GraphQLUtils.getNumberOfQueriesOrMutations("query", queryGraph)}").append(System.getProperty("line.separator"))
                buffer.append("The number of fields in all nodes, excluding Query: ${GraphQLUtils.getNumberOfFields(queryGraph)}").append(System.getProperty("line.separator"))
                buffer.append("The number of edge (number of fields that are pointers to other nodes in the graph) including Query: ${GraphQLUtils.getNumberOfEdges(queryGraph)}").append(System.getProperty("line.separator"))
                buffer.append("The number of nodes, including Query: ${GraphQLUtils.getGraphSize(queryGraph)}").append(System.getProperty("line.separator"))
                GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "query", queryGraph, paths)
                buffer.append("The longest path is: ${GraphQLUtils.longestPath(paths)}").append(System.getProperty("line.separator"))
                buffer.append("The number of unions: ${GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.UNION_TAG, queryGraph)}").append(System.getProperty("line.separator"))
                buffer.append("The number of interfaces: ${GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.INTERFACE_TAG, queryGraph)}").append(System.getProperty("line.separator"))
                buffer.append("The maximum path of the minimum among all entry points: ${GraphQLUtils.maxPathAmongAllEntryPointsForAllNodes(GraphQLUtils.minPathAmongAllEntryPointsForEachNode(GraphQLUtils.getShortestPathFromEachEntryPointToEachNode(queryGraph)))}").append(System.getProperty("line.separator"))
            } else {
                GraphQLUtils.constructGraph(state, "querytype", " ", queryGraph, mutableListOf(), mutableSetOf())
                if (GraphQLUtils.getGraphSize(queryGraph) != 0) {
                    buffer.append("The number of fields in the type Query: ${GraphQLUtils.getNumberOfQueriesOrMutations("querytype", queryGraph)}").append(System.getProperty("line.separator"))
                    buffer.append("The number of fields in all nodes, excluding Query: ${GraphQLUtils.getNumberOfFields(queryGraph)}").append(System.getProperty("line.separator"))
                    buffer.append("The number of edge (number of fields that are pointers to other nodes in the graph) including Query: ${GraphQLUtils.getNumberOfEdges(queryGraph)}").append(System.getProperty("line.separator"))
                    buffer.append("The number of nodes, including Query: ${GraphQLUtils.getGraphSize(queryGraph)}").append(System.getProperty("line.separator"))
                    GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "querytype", queryGraph, paths)
                    buffer.append("The longest path is: ${GraphQLUtils.longestPath(paths)}").append(System.getProperty("line.separator"))
                    buffer.append("The number of unions: ${GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.UNION_TAG, queryGraph)}").append(System.getProperty("line.separator"))
                    buffer.append("The number of interfaces: ${GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.INTERFACE_TAG, queryGraph)}").append(System.getProperty("line.separator"))
                    buffer.append("The maximum path of the minimum among all entry points: ${GraphQLUtils.maxPathAmongAllEntryPointsForAllNodes(GraphQLUtils.minPathAmongAllEntryPointsForEachNode(GraphQLUtils.getShortestPathFromEachEntryPointToEachNode(queryGraph)))}").append(System.getProperty("line.separator"))

                } else {
                    GraphQLUtils.constructGraph(state, "root", " ", queryGraph, mutableListOf(), mutableSetOf())
                    if (GraphQLUtils.getGraphSize(queryGraph) != 0) {
                        buffer.append("The number of fields in the type Query: ${GraphQLUtils.getNumberOfQueriesOrMutations("root", queryGraph)}").append(System.getProperty("line.separator"))
                        buffer.append("The number of fields in all nodes, excluding Query: ${GraphQLUtils.getNumberOfFields(queryGraph)}").append(System.getProperty("line.separator"))
                        buffer.append("The number of edge (number of fields that are pointers to other nodes in the graph) including Query: ${GraphQLUtils.getNumberOfEdges(queryGraph)}").append(System.getProperty("line.separator"))
                        buffer.append("The number of nodes, including Query: ${GraphQLUtils.getGraphSize(queryGraph)}").append(System.getProperty("line.separator"))
                        GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "root", queryGraph, paths)
                        buffer.append("The longest path is: ${GraphQLUtils.longestPath(paths)}").append(System.getProperty("line.separator"))
                        buffer.append("The number of unions: ${GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.UNION_TAG, queryGraph)}").append(System.getProperty("line.separator"))
                        buffer.append("The number of interfaces: ${GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.INTERFACE_TAG, queryGraph)}").append(System.getProperty("line.separator"))
                        buffer.append("The maximum path of the minimum among all entry points: ${GraphQLUtils.maxPathAmongAllEntryPointsForAllNodes(GraphQLUtils.minPathAmongAllEntryPointsForEachNode(GraphQLUtils.getShortestPathFromEachEntryPointToEachNode(queryGraph)))}").append(System.getProperty("line.separator"))

                    }
                }
            }

            val mutationGraph: MutableMap<String, GraphQLUtils.GraphInfo> = mutableMapOf()
            visitedVertex = mutableSetOf()
            stack = ArrayDeque<String>()
            paths = mutableListOf()

            GraphQLUtils.constructGraph(state, "mutation", " ", mutationGraph, mutableListOf(), mutableSetOf())
            buffer.append("--**--").append(System.getProperty("line.separator"))
            buffer.append("The number of fields in all nodes, excluding Mutation: ${GraphQLUtils.getNumberOfFields(mutationGraph)}").append(System.getProperty("line.separator"))
            buffer.append("The number of edge (number of fields that are pointers to other nodes in the graph) including Mutation: ${GraphQLUtils.getNumberOfEdges(mutationGraph)}").append(System.getProperty("line.separator"))
            buffer.append("The number of nodes, including Mutation: ${GraphQLUtils.getGraphSize(mutationGraph)}").append(System.getProperty("line.separator"))
            buffer.append("The number of unions: ${GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.UNION_TAG, mutationGraph)}").append(System.getProperty("line.separator"))
            buffer.append("The number of interfaces: ${GraphQLUtils.getNumberOfUnionOrInterface(GqlConst.INTERFACE_TAG, mutationGraph)}").append(System.getProperty("line.separator"))

            if (GraphQLUtils.checkNodeExists("mutation", mutationGraph)) {
                buffer.append("The number of fields in the type Mutation: ${GraphQLUtils.getNumberOfQueriesOrMutations("mutation", mutationGraph)}").append(System.getProperty("line.separator"))
                GraphQLUtils.getAllPathsFromEntryPoint(visitedVertex, stack, "mutation", mutationGraph, paths)
                buffer.append("The longest path is: ${GraphQLUtils.longestPath(paths)}").append(System.getProperty("line.separator"))
                buffer.append("The maximum path of the minimum among all entry points: ${GraphQLUtils.maxPathAmongAllEntryPointsForAllNodes(GraphQLUtils.minPathAmongAllEntryPointsForEachNode(GraphQLUtils.getShortestPathFromEachEntryPointToEachNode(mutationGraph)))}").append(System.getProperty("line.separator"))

            }

        }
        File("src/test/resources/graphql/stats").bufferedWriter().use { out -> out.append(buffer) }

    }
}