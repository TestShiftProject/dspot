package eu.stamp_project.prettifier.testnaming.code2vec;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.api.Assertions;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 18/02/19
 */
public class Code2VecExecutorTest {

    /*
        These test methods requires to have code2vec in src/test/resources
     */

    @Disabled // DOES NOT WORK ON TRAVIS, CANNOT FIND python3 cmd
    @Test
    public void test() {
        ;
        final Code2VecExecutor code2VecExecutor = new Code2VecExecutor(
                "src/test/resources/code2vec/code2vec",
                "../model/saved_model_iter20",
                3000
        );
        code2VecExecutor.run();
        final String output = code2VecExecutor.getOutput();
        System.out.println(output);
        Assertions.assertTrue(output.startsWith(STARTS_WITH));
        Assertions.assertTrue(output.contains(CONTAINS));
    }

    private static final String STARTS_WITH = "Original name:\tf";
    private static final String CONTAINS = "\t(1.000000) predicted: ['test']";
}
