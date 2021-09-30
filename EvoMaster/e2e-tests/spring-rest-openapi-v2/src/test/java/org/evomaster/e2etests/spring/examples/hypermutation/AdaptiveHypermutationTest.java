package org.evomaster.e2etests.spring.examples.hypermutation;

import com.foo.rest.examples.spring.hypermutation.HighWeightRestController;
import org.evomaster.core.problem.rest.RestIndividual;
import org.evomaster.core.search.Solution;
import org.evomaster.e2etests.spring.examples.SpringTestBase;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertTrue;


public class AdaptiveHypermutationTest extends HypermutationTestBase {

    @BeforeAll
    public static void initClass() throws Exception {

        SpringTestBase.initClass(new HighWeightRestController(Arrays.asList("/api/highweight/differentWeight")));
    }

    @Test
    public void testRunAdaptiveHypermutation() throws Throwable {

        runTestHandlingFlakyAndCompilation(
                "hypermtation/TestLowWeightHighImpact",
                "org.adaptivehypermuation.LowWeightHighImpactTest",
                2000,
                true,
                (args) -> {

                    args.add("--weightBasedMutationRate");
                    args.add("true");

                    args.add("--probOfArchiveMutation");
                    args.add("1.0");
                    args.add("--adaptiveGeneSelectionMethod");
                    args.add("APPROACH_IMPACT");
                    args.add("--archiveGeneMutation");
                    args.add("SPECIFIED_WITH_SPECIFIC_TARGETS");
                    args.add("--enableTrackEvaluatedIndividual");
                    args.add("true");


                    args.add("--probOfRandomSampling");
                    args.add("0.0");

                    Solution<RestIndividual> solution = initAndRun(args);


                    boolean ok = solution.getIndividuals().stream().allMatch(s-> check(s, "lowWeightHighCoverage",1));
                    assertTrue(ok);

                }, 4);
    }



}
