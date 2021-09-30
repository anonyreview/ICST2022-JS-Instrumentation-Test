package org.evomaster.core.search.service

import java.util.concurrent.atomic.AtomicInteger


/**
 * To represent and identify a coverage target, we use numeric ids.
 * But those are not very "descriptive" of what the targets actually are.
 * So, we need a mapping to a String description (which itself would be
 * a unique id).
 * Note: we do not use these strings directly as it would be too inefficient.
 * Furthermore, as the ids are passed as query parameters in HTTP GET requests,
 * there are limits on length
 */
class IdMapper {

    companion object {

        private const val FAULT_DESCRIPTIVE_ID_PREFIX = "PotentialFault_"

        private const val FAULT_500 = "500_"

        private const val FAULT_PARTIAL_ORACLE = "PartialOracle_"

        private const val GQL_ERRORS_PREFIX = "GQL_ERRORS_ACTION"

        private const val GQL_ERRORS_LINE_PREFIX = "GQL_ERRORS_LINE"

        private const val GQL_NO_ERRORS = "GQL_NO_ERRORS"

        fun isFault(descriptiveId: String) = descriptiveId.startsWith(FAULT_DESCRIPTIVE_ID_PREFIX) || isGQLErrors(descriptiveId, true)

        fun isFault500(descriptiveId: String) = descriptiveId.startsWith(FAULT_DESCRIPTIVE_ID_PREFIX+ FAULT_500)

        fun isFaultPartialOracle(descriptiveId: String) = descriptiveId.startsWith(FAULT_DESCRIPTIVE_ID_PREFIX+ FAULT_PARTIAL_ORACLE)

        fun isGQLErrors(descriptiveId: String, withLine: Boolean = false) =
                if (!withLine) descriptiveId.startsWith(GQL_ERRORS_PREFIX)
                else descriptiveId.startsWith(GQL_ERRORS_LINE_PREFIX)

        fun isGQLNoErrors(descriptiveId: String) = descriptiveId.startsWith(GQL_NO_ERRORS)

        fun faultInfo(descriptiveId: String) : String{
            if(! isFault(descriptiveId)){
                throw IllegalArgumentException("Invalid non-fault input id: $descriptiveId")
            }
            return descriptiveId.substring(FAULT_DESCRIPTIVE_ID_PREFIX.length)
        }

        fun isLocal(id: Int): Boolean = id < 0
    }

    private val mapping: MutableMap<Int, String> = mutableMapOf()

    private val reverseMapping: MutableMap<String, Int> = mutableMapOf()

    /**
     * Counter used to create local id, based on the return values
     * of the interactions with the SUT.
     * The counter has to be negative to avoid collisions with
     * ids generated on the SUT (eg, via bytecode instrumentation
     * monitoring)
     */
    private val localCounter = AtomicInteger(-1)

    fun addMapping(id: Int, descriptiveId: String) {
        mapping[id] = descriptiveId
        reverseMapping[descriptiveId] = id
    }

    fun getDescriptiveId(id: Int) = mapping[id] ?: "undefined"

    fun handleLocalTarget(descriptiveId: String): Int {
        return reverseMapping.getOrPut(descriptiveId, {
            val k = localCounter.decrementAndGet()
            mapping[k] = descriptiveId
            k
        })
    }

    fun getFaultDescriptiveIdFor500(postfix: String): String {
        return FAULT_DESCRIPTIVE_ID_PREFIX + FAULT_500 + postfix
    }

    fun getFaultDescriptiveIdForPartialOracle(postfix: String): String {
        return FAULT_DESCRIPTIVE_ID_PREFIX + FAULT_PARTIAL_ORACLE + postfix
    }

    /*
        TODO double-check
        Is just using "method" name enough to identify a Query/Mutation in GQL?
        or could we get issue when there is name overloading? ie, queries with same
        name but different input signature.
        if so, should use a description of the input signatures to get unique ids...
        although likely this is really loooow priority
     */

    fun getGQLErrorsDescriptiveWithMethodName(method: String) = "$GQL_ERRORS_PREFIX:$method"

    fun getGQLErrorsDescriptiveWithMethodNameAndLine(line : String, method: String) = "${GQL_ERRORS_LINE_PREFIX}:${method}_$line"

    fun getGQLNoErrors(method: String) = "$GQL_NO_ERRORS:$method"

    fun isFault(id: Int) : Boolean = mapping[id]?.let{ isFault(it)} ?: false

    fun isFault500(id: Int): Boolean = mapping[id]?.let {isFault500(it)} ?: false

    fun isFaultExpectation(id: Int): Boolean = mapping[id]?.let{ isFaultPartialOracle(it) } ?:false

    fun isGQLErrors(id : Int, withLine: Boolean) : Boolean = mapping[id]?.let { isGQLErrors(it, withLine) } == true

    fun isGQLNoErrors(id : Int) : Boolean = mapping[id]?.let { isGQLNoErrors(it) } == true
}