package org.evomaster.core.search

import org.evomaster.core.EMConfig
import org.evomaster.core.database.DatabaseExecution
import org.evomaster.core.EMConfig.SecondaryObjectiveStrategy.*
import org.evomaster.core.search.service.IdMapper
import org.evomaster.core.search.service.mutator.EvaluatedMutation
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min

/**
As the number of targets is unknown, we cannot have
a minimization problem, as new targets could be added
throughout the search
 */
class FitnessValue(
        /** An estimation of the size of the individual that obtained
         * this fitness value. Longer individuals are worse, but only
         * when fitness is not strictly better */
        var size: Double) {

    init {
        if (size < 0.0) {
            throw IllegalArgumentException("Invalid size value: $size")
        }
    }

    companion object {

        const val MAX_VALUE = 1.0

        fun isMaxValue(value: Double) = value == MAX_VALUE

        private val log: Logger = LoggerFactory.getLogger(FitnessValue::class.java)
    }

    /**
     *  Key -> target Id
     *
     *  Value -> heuristic distance in [0,1], where 1 is for "covered"
     */
    private val targets: MutableMap<Int, Heuristics> = mutableMapOf()

    /**
     *  Key -> action Id
     *
     * Value -> List of extra heuristics to minimize (min 0).
     * Those are related to the whole test, and not specific target.
     * Covering those extra does not guarantee that it would help in
     * covering target.
     * An example is rewarding SQL Select commands that return non-empty
     *
     * Note: these values are SORTED.
     */
    private val extraToMinimize: MutableMap<Int, List<Double>> = mutableMapOf()


    /**
     * Key -> action Id
     *
     * Value -> info on how the SQL database was accessed
     */
    private val databaseExecutions: MutableMap<Int, DatabaseExecution> = mutableMapOf()

    /**
     * When SUT does SQL commands using WHERE, keep track of when those "fails" (ie evaluate
     * to false), in particular the tables and columns in them involved
     */
    private val aggregatedFailedWhere: MutableMap<String, Set<String>> = mutableMapOf()

    /**
    * How long it took to evaluate this fitness value.
    */
    var executionTimeMs : Long = Long.MAX_VALUE


    fun copy(): FitnessValue {
        val copy = FitnessValue(size)
        copy.targets.putAll(this.targets)
        copy.extraToMinimize.putAll(this.extraToMinimize)
        copy.databaseExecutions.putAll(this.databaseExecutions) //note: DatabaseExecution supposed to be immutable
        copy.aggregateDatabaseData()
        copy.executionTimeMs = executionTimeMs
        return copy
    }

    /**
     * We keep track of DB interactions per action.
     * However, there are cases in which we only care of aggregated data for all actions.
     * Instead of re-computing them each time, we just do it once and save the results
     */
    fun aggregateDatabaseData(){

        aggregatedFailedWhere.clear()
        aggregatedFailedWhere.putAll(DatabaseExecution.mergeData(
                databaseExecutions.values,
                {x ->  x.failedWhere}
        ))
    }

    fun setExtraToMinimize(actionIndex: Int, list: List<Double>) {
        extraToMinimize[actionIndex] = list.sorted()
    }

    fun setDatabaseExecution(actionIndex: Int, databaseExecution: DatabaseExecution){
        databaseExecutions[actionIndex] = databaseExecution
    }

    fun isAnyDatabaseExecutionInfo() = databaseExecutions.isNotEmpty()

    fun getViewOfData(): Map<Int, Heuristics> {
        return targets
    }

    fun getViewOfAggregatedFailedWhere() = aggregatedFailedWhere

    fun doesCover(target: Int): Boolean {
        return targets[target]?.distance == MAX_VALUE
    }

    fun getHeuristic(target: Int): Double = targets[target]?.distance ?: 0.0

    fun reachedTargets() : Set<Int> = getViewOfData().filter { it.value.distance > 0.0 }.keys

    fun computeFitnessScore(): Double {

        return targets.values.map { h -> h.distance }.sum()
    }

    fun computeFitnessScore(targetIds : List<Int>): Double {

        return targets.filterKeys { targetIds.contains(it)}.values.map { h -> h.distance }.sum()
    }

    fun coveredTargets(): Int {

        return targets.values.filter { t -> t.distance == MAX_VALUE }.count()
    }

    fun coveredTargets(prefix: String, idMapper: IdMapper) : Int{

        return targets.entries
                .filter { it.value.distance == MAX_VALUE }
                .filter { idMapper.getDescriptiveId(it.key).startsWith(prefix) }
                .count()
    }

    fun coverTarget(id: Int) {
        updateTarget(id, MAX_VALUE)
    }

    fun gqlErrors(idMapper: IdMapper, withLine : Boolean): List<String>{
        // GQLErrors would be >0 when it is initialed, so we count it when it is covered.
        return targets.filter { it.value.distance == MAX_VALUE }.keys
                .filter { idMapper.isGQLErrors(it, withLine) }
                .map { idMapper.getDescriptiveId(it) }
    }

    fun gqlNoErrors(idMapper: IdMapper): List<String>{
        // GQLNoErrors would be >0 when it is initialed, so we count it when it is covered.
        return targets.filter { it.value.distance == MAX_VALUE }.keys
                .filter { idMapper.isGQLNoErrors(it) }
                .map { idMapper.getDescriptiveId(it) }
    }

    fun potentialFoundFaults(idMapper: IdMapper) : List<String>{
        return targets.keys
                .filter { idMapper.isFault(it)}
                .map { idMapper.getDescriptiveId(it) }
    }

    fun potential500Faults(idMapper: IdMapper): List<String>{
        return targets.keys
                .filter{ idMapper.isFault500(it)}
                .map{idMapper.getDescriptiveId(it)}
    }

    fun potentialPartialOracleFaults(idMapper: IdMapper): List<String>{
        return targets.keys
                .filter{idMapper.isFaultExpectation(it)}
                .map{idMapper.getDescriptiveId(it)}
    }

    /**
     * Set the heuristic [value] for the given target [id].
     * If already existing, replace it only if better.
     */
    fun updateTarget(id: Int, value: Double, actionIndex : Int = -1) {

        if (value < 0 || value > MAX_VALUE) {
            throw IllegalArgumentException("Invalid value: $value")
        }

        val current = targets[id]

        if(current == null || value > current.distance) {
            targets[id] = Heuristics(value, actionIndex)
        }
    }

    /**
     * This only merges the target heuristics, and not the extra ones
     */
    fun merge(other: FitnessValue) {

        other.targets.keys.forEach { t ->
            val k = other.getHeuristic(t)
            if (k > this.getHeuristic(t)) {
                this.updateTarget(t, k)
            }
        }
    }

    /**
     * Check if current does subsume [other].
     * This means covering at least the same targets, and at least one better or
     * one more.
     *
     * Recall: during the search, we might not calculate all targets, eg once they
     * are covered.
     *
     * This keeps into account both test size and its execution time
     *
     * @param other, the one we compare to
     * @param targetSubset, only calculate subsumption on these testing targets
     */
    fun subsumes(
            other: FitnessValue,
            targetSubset: Set<Int>,
            strategy: EMConfig.SecondaryObjectiveStrategy,
            bloatControlForSecondaryObjective: Boolean,
            minimumSize: Int,
            useTimestamps: Boolean)
            : Boolean {

        var atLeastOneBetter = false

        for (k in targetSubset) {

            val v = this.targets[k]?.distance ?: 0.0
            val z = other.targets[k]?.distance ?: 0.0
            if (v < z) {
                //  if it is worse on any target, then it cannot be subsuming
                if (log.isTraceEnabled){
                    log.trace("for target {}, subsume is false with v ({}) < z ({})", k, v, z)
                }
                return false
            }

            atLeastOneBetter = atLeastOneBetter || betterThan(k, other, strategy, bloatControlForSecondaryObjective, minimumSize)
        }

        if(atLeastOneBetter){
            if (log.isTraceEnabled){
                log.trace("subsume is true with atLeastOneBetter")
            }
            return true
        }

        if(useTimestamps && other.executionTimeMs != Long.MAX_VALUE &&
                executionTimeMs < other.executionTimeMs * 2){
            /*
                time is very, very tricky to handle... as its evaluation
                is not fully deterministic, ie, can have noise.
                so, if for any reason two fitnesses are equivalent under all
                other heuristics, then we say one subsumes the other if twice
                as fast to compute
             */
            if (log.isTraceEnabled){
                log.trace("subsume is true with useTimestamps, and current is {}, other is {}", executionTimeMs, other.executionTimeMs)
            }
            return true
        }
        if (log.isTraceEnabled){
            log.trace("subsume is false at the end")
        }
        return false
    }

    fun subsumes(
            other: FitnessValue,
            targetSubset: Set<Int>,
            config : EMConfig)
            : Boolean {

        return subsumes(other,
                targetSubset,
                config.secondaryObjectiveStrategy,
                config.bloatControlForSecondaryObjective,
                config.minimumSizeControl,
                config.useTimeInFeedbackSampling)
    }

    /**
     * @return [this] is better than [other] for [target].
     */
    fun betterThan(target: Int, other: FitnessValue, strategy: EMConfig.SecondaryObjectiveStrategy, bloatControlForSecondaryObjective: Boolean, minimumSize: Int) : Boolean{
        val z = other.getHeuristic(target)
        val v = getHeuristic(target)
        if (v < z) {
            if (log.isTraceEnabled){
                log.trace("for target {}, betterThan is false with v ({}) < z ({})", target, v, z)
            }
            return false
        }

        val extra = compareExtraToMinimize(target, other, strategy)

        return betterThan(target =target, heuristics = z, size = other.size, extra = extra, minimumSize = minimumSize, bloatControlForSecondaryObjective = bloatControlForSecondaryObjective)
    }

    /**
     * @return [this] equivalent with [other] for [target].
     */
    fun equivalent(target: Int, other: FitnessValue, strategy: EMConfig.SecondaryObjectiveStrategy) : Boolean{
        val z = other.getHeuristic(target)
        val v = getHeuristic(target)
        if (z != v) return false

        val extra = compareExtraToMinimize(target, other, strategy)

        //WARN: cannot really do this unless we update betterThan as well.
        //      But unclear if really make sense when considering specific targets
        //Time is very tricky... so we consider equivalent as long as not more than twice time difference
//        val timeRatio = if(this.executionTimeMs == Long.MAX_VALUE ||
//                other.executionTimeMs == Long.MAX_VALUE ||
//                (this.executionTimeMs == 0L && other.executionTimeMs==0L)) {
//            0.0
//        }else {
//                abs(this.executionTimeMs - other.executionTimeMs) /
//                        min(this.executionTimeMs.toDouble(), other.executionTimeMs.toDouble())
//        }

        return extra == 0
                && this.size == other.size
//                && timeRatio < 1.0
    }

    private fun betterThan(target: Int, heuristics: Double, size: Double, extra: Int, bloatControlForSecondaryObjective: Boolean, minimumSize: Int) : Boolean{

        if (log.isTraceEnabled){
            log.trace("for target{}, checking betterThan with extras and extra is {}", target, extra)
        }

        val v = getHeuristic(target)
        if (v < heuristics) {

            if (log.isTraceEnabled){
                log.trace("for target{}, betterThan with extras is false with v ({}) < heuristics ({})", target, v, heuristics)
            }
            return false

        }

        return (if(bloatControlForSecondaryObjective

            && min(this.size, size) >= minimumSize){
            v > heuristics ||
                    (v == heuristics && this.size <  size) ||
                    (v == heuristics &&  this.size ==  size && extra > 0)
        } else {
            v > heuristics ||
                    (v == heuristics && extra > 0) ||
                    (v == heuristics && extra == 0 && this.size <  size)
        }).also {
            if (log.isTraceEnabled){
                log.trace("for target{}, betterThan with extras is {} ", target, it)
            }
        }
    }

    fun reachMoreTargets(other: FitnessValue) = targets.size > other.getViewOfData().size


    /**
     * @param other, the one we compare to
     * @param targetSubset, only calculate subsumption on these testing targets
     * @param targetInfo, comparison results for each of [targetSubset]. It will be updated
     *                  as side-effect of this function
     * @param config, includes setting for comparison
     */
    fun computeDifference(
            other: FitnessValue,
            targetSubset: Set<Int>,
            targetInfo: MutableMap<Int, EvaluatedMutation>,
            config: EMConfig)  {

        for (k in targetSubset) {
            val value = when {
                betterThan(target = k, other = other, strategy = config.secondaryObjectiveStrategy,bloatControlForSecondaryObjective = config.bloatControlForSecondaryObjective, minimumSize = config.minimumSizeControl) -> EvaluatedMutation.WORSE_THAN
                equivalent(k, other, strategy = config.secondaryObjectiveStrategy) -> EvaluatedMutation.EQUAL_WITH
                else -> EvaluatedMutation.BETTER_THAN
            }
            targetInfo.merge(k, value){ old, new -> if (old.value > new.value) old else new }
        }
    }

    fun averageExtraDistancesToMinimize(actionIndex: Int): Double{
        return averageDistance(extraToMinimize[actionIndex])
    }

    /**
     * Compare the extra heuristics between this and [other].
     *
     * @return 0 if equivalent, 1 if this is better, and -1 otherwise
     */
    fun compareExtraToMinimize(
            target: Int,
            other: FitnessValue,
            strategy: EMConfig.SecondaryObjectiveStrategy)
            : Int {

        return when(strategy){
            AVG_DISTANCE -> compareAverage(target, other)
            AVG_DISTANCE_SAME_N_ACTIONS -> compareAverageSameNActions(target, other)
            BEST_MIN -> compareByBestMin(target, other)
        }
    }


    private fun isEmptyList(list: List<Double>?) : Boolean{
        return list == null || list.isEmpty()
    }

    private fun averageDistance(distances: List<Double>?): Double {
        if (isEmptyList(distances)) {
            //return 0.0
            throw IllegalArgumentException("Cannot compute average on empty list")
        }

        val sum = distances!!.map { v -> v / distances.size }.sum()

        return sum
    }

    private fun compareAverageSameNActions(target: Int, other: FitnessValue): Int {

        val thisAction = targets[target]?.actionIndex
        val otherAction = other.targets[target]?.actionIndex

        /*
            [non-determinism-source] Man: a SQL command might be invoked multiple times, see [makeHttpCall] in RemoteController
            this might cause non-determinism results for [thisN] and [otherN]
        */
        val thisN = databaseExecutions[thisAction]?.numberOfSqlCommands ?: 0
        val otherN = other.databaseExecutions[otherAction]?.numberOfSqlCommands ?: 0

        if (log.isTraceEnabled){
            log.trace("compareAverageSameNActions with thisN {} and otherN {}", thisN, otherN)
        }

        return when {
            thisN > otherN -> 1
            thisN < otherN -> -1
            else -> compareAverage(target, other)
        }
    }

    private fun compareAverage(target: Int, other: FitnessValue): Int {

        val thisAction = targets[target]?.actionIndex
        val otherAction = other.targets[target]?.actionIndex

        val thisDistances = this.extraToMinimize[thisAction]
        val otherDistances = other.extraToMinimize[otherAction]


        if (log.isTraceEnabled){
            log.trace("compareAverage with thisAction {} and otherAction {}", thisAction, otherAction)
        }

        if (log.isTraceEnabled){
            log.trace("compareAverage with thisDistances {} and otherDistances {}", thisDistances, otherDistances)
        }

        if(isEmptyList(thisDistances) && isEmptyList(otherDistances)){
            return 0
        }
        if(!isEmptyList(thisDistances) && isEmptyList(otherDistances)){
            return +1
        }
        if(isEmptyList(thisDistances) && !isEmptyList(otherDistances)){
            return -1
        }


        val ts = averageDistance(thisDistances)
        val os = averageDistance(otherDistances)

        return when {
            ts < os -> +1
            ts > os -> -1
            else -> 0
        }
    }


    private fun compareByBestMin(target: Int, other: FitnessValue): Int {

        val thisAction = targets[target]?.actionIndex
        val otherAction = other.targets[target]?.actionIndex

        val thisLength = this.extraToMinimize[thisAction]?.size ?: 0
        val otherLength = other.extraToMinimize[otherAction]?.size ?: 0
        val minLen = min(thisLength, otherLength)

        if (minLen > 0) {
            for (i in 0 until minLen) {
                val te = this.extraToMinimize[thisAction]!![i]
                val oe = other.extraToMinimize[otherAction]!![i]

                /*
                    We prioritize the improvement of lowest
                    heuristics, as more likely to be covered (ie 0)
                    first.
                */

                if (te < oe) {
                    return +1
                } else if (te > oe) {
                    return -1
                }
            }
        }

        val thisN = databaseExecutions[thisAction]?.numberOfSqlCommands ?: 0
        val otherN = other.databaseExecutions[otherAction]?.numberOfSqlCommands ?: 0

        /*
            if same min, reward number of SQL commands: if more, the
            better.
            If even with that we cannot make a choice, then reward
            the one with less heuristics, as that mean it had more
            success with the SQL commands
         */

        return when {
            thisN > otherN -> 1
            thisN < otherN -> -1
            thisLength > otherLength -> -1
            thisLength < otherLength -> 1
            else -> 0
        }
    }

    /**
     * @return targets that are reached/covered by an action at [actionIndex]
     */
    fun getTargetsByAction(actionIndex : Int) : Set<Int> {
        return targets.filterValues { it.actionIndex == actionIndex }.keys
    }
}