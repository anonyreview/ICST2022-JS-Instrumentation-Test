package org.evomaster.core.search.impact.impactinfocollection.value

import org.evomaster.core.search.gene.Gene
import org.evomaster.core.search.gene.ObjectGene
import org.evomaster.core.search.impact.impactinfocollection.*

/**
 * created by manzh on 2019-09-09
 */
class ObjectGeneImpact  (
        sharedImpactInfo: SharedImpactInfo,
        specificImpactInfo: SpecificImpactInfo,
        val fields : MutableMap<String, Impact> = mutableMapOf()
) : GeneImpact(sharedImpactInfo, specificImpactInfo){

    constructor(id: String, objectGene: ObjectGene) : this (SharedImpactInfo(id), SpecificImpactInfo(), fields = objectGene.fields.map { Pair(it.name, ImpactUtils.createGeneImpact(it, it.name)) }.toMap().toMutableMap())

    override fun copy(): ObjectGeneImpact {
        return ObjectGeneImpact(
                shared.copy(),
                specific.copy(),
                fields = fields.map { Pair(it.key, it.value.copy()) }.toMap().toMutableMap())
    }

    override fun clone(): ObjectGeneImpact {
        return ObjectGeneImpact(
                shared.clone(),
                specific.clone(),
                fields.map { it.key to it.value.clone() }.toMap().toMutableMap()
        )
    }

    override fun countImpactWithMutatedGeneWithContext(gc: MutatedGeneWithContext, noImpactTargets: Set<Int>, impactTargets: Set<Int>, improvedTargets: Set<Int>, onlyManipulation: Boolean) {

        countImpactAndPerformance(noImpactTargets = noImpactTargets, impactTargets = impactTargets, improvedTargets = improvedTargets, onlyManipulation = onlyManipulation, num = gc.numOfMutatedGene)
        if (gc.previous == null && impactTargets.isNotEmpty()) return
        if (gc.current !is ObjectGene)
            throw IllegalArgumentException("gc.current ${gc.current::class.java.simpleName} should be ObjectGene")
        if (gc.previous == null){
            gc.current.fields.forEach {
                val fImpact = fields.getValue(it.name) as? GeneImpact?:throw IllegalArgumentException("impact should be gene impact")
                val mutatedGeneWithContext = MutatedGeneWithContext(previous = null, current =  it, action = "none", position = -1, numOfMutatedGene = gc.current.fields.size)
                fImpact.countImpactWithMutatedGeneWithContext(mutatedGeneWithContext, noImpactTargets = noImpactTargets, impactTargets = impactTargets, improvedTargets = improvedTargets, onlyManipulation = onlyManipulation)
            }
            return
        }
        if (gc.previous !is ObjectGene)
            throw IllegalArgumentException("gc.previous ${gc.previous::class.java.simpleName} should be ObjectGene")


        val mutatedFields = gc.current.fields.zip(gc.previous.fields) { cf, pf ->
            Pair(Pair(cf, pf), cf.containsSameValueAs(pf))
        }.filter { !it.second }.map { it.first }

        val onlyManipulation = mutatedFields.size > 1 && impactTargets.isNotEmpty()

        mutatedFields.forEach {g->
            val fImpact = fields.getValue(g.first.name) as? GeneImpact?:throw IllegalArgumentException("impact should be gene impact")
            val mutatedGeneWithContext = MutatedGeneWithContext(previous = g.second, current =  g.first, action = "none", position = -1, numOfMutatedGene = gc.numOfMutatedGene * mutatedFields.size)
            fImpact.countImpactWithMutatedGeneWithContext(mutatedGeneWithContext, noImpactTargets = noImpactTargets, impactTargets = impactTargets, improvedTargets = improvedTargets, onlyManipulation = onlyManipulation)
        }
    }

    override fun validate(gene: Gene): Boolean = gene is ObjectGene

    override fun flatViewInnerImpact(): Map<String, Impact> {
        val map = mutableMapOf<String, Impact>()
        fields.forEach { (t, u) ->
            map.putIfAbsent("${getId()}-$t", u)
            if (u is GeneImpact && u.flatViewInnerImpact().isNotEmpty())
                map.putAll(u.flatViewInnerImpact())
        }
        return map
    }

    override fun innerImpacts(): List<Impact> {
        return fields.values.toList()
    }

    override fun syncImpact(previous: Gene?, current: Gene) {
        check(previous,current)

        (current as ObjectGene).fields.forEach { f ->
            if (!fields.containsKey(f.name)){
                fields.putIfAbsent(f.name, ImpactUtils.createGeneImpact(f, f.name))
            }
        }

        fields.forEach { (t, u) ->
            val c = current.fields.find { it.name == t }?: throw IllegalArgumentException("the matched field for impact cannot be found")
            val p = (previous as? ObjectGene)?.fields?.find { it.name == t }
            (u as GeneImpact).syncImpact(p, c)
        }

    }
}