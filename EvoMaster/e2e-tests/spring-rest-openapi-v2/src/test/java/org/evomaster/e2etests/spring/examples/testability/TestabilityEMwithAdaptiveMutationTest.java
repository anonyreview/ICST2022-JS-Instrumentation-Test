package org.evomaster.e2etests.spring.examples.testability;

import com.foo.rest.examples.spring.testability.TestabilityController;
import org.evomaster.core.EMConfig;
import org.evomaster.core.problem.rest.HttpVerb;
import org.evomaster.core.problem.rest.RestIndividual;
import org.evomaster.core.search.Solution;
import org.evomaster.e2etests.spring.examples.SpringTestBase;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * created by manzh on 2020-07-01
 */
public class TestabilityEMwithAdaptiveMutationTest extends SpringTestBase {

    @BeforeAll
    public static void initClass() throws Exception {
        SpringTestBase.initClass(new TestabilityController());
    }

    @Test
    void testRunEM() throws Throwable {

        defaultSeed = 0;

        runTestHandlingFlakyAndCompilation(
                "TestabilityEM",
                "org.bar.TestabilityEMAGM",
                8_000,
                true,
                (args) -> {

                    args.add("--baseTaintAnalysisProbability");
                    args.add("0.9");

                    args.add("--enableTrackEvaluatedIndividual");
                    args.add("true");

                    args.add("--weightBasedMutationRate");
                    args.add("true");

                    args.add("--d");
                    args.add("0.8");

                    args.add("--adaptiveGeneSelectionMethod");
                    args.add("APPROACH_IMPACT");

                    args.add("--archiveGeneMutation");
                    args.add("SPECIFIED_WITH_SPECIFIC_TARGETS");

                    args.add("--probOfArchiveMutation");
                    args.add("0.5");

                    args.add("--enableWeightBasedMutationRateSelectionForGene");
                    args.add("true");


                    Solution<RestIndividual> solution = initAndRun(args);

                    assertTrue(solution.getIndividuals().size() >= 1);

                    /*
                        there seem exist some dependency among tests. After executing the first test, SUT fails to throw the exception in following two tests.
                     */
                    assertHasAtLeastOne(solution, HttpVerb.GET, 500, "/api/testability/{date}/{number}/{setting}", null);
                    assertHasAtLeastOne(solution, HttpVerb.GET, 200, "/api/testability/{date}/{number}/{setting}", "ERROR");
                    assertHasAtLeastOne(solution, HttpVerb.GET, 200, "/api/testability/{date}/{number}/{setting}", "OK");
                },
                10);
    }
}
