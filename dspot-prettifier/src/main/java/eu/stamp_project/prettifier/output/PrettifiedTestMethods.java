package eu.stamp_project.prettifier.output;

import eu.stamp_project.dspot.common.miscellaneous.CloneHelper;
import eu.stamp_project.dspot.common.miscellaneous.DSpotUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtType;

import java.io.File;
import java.util.List;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 22/02/19
 */
public class PrettifiedTestMethods {

    private static final Logger LOGGER = LoggerFactory.getLogger(PrettifiedTestMethods.class);

    private final String outputDirectory;

    public PrettifiedTestMethods(String outputDirectory) {
        this.outputDirectory = outputDirectory;
    }

    public void output(
            CtType<?> testClass,
            List<CtMethod<?>> prettifiedAmplifiedTestMethods) {
        CtType<?> finalTestClass = CloneHelper.cloneTestClassRemoveOldTestsAndAddGivenTest(testClass, prettifiedAmplifiedTestMethods);
        LOGGER.info(finalTestClass.toString());
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        DSpotUtils.printCtTypeToGivenDirectory(finalTestClass, new File(outputDirectory), true);
        LOGGER.info("Print {} in {}", finalTestClass.getQualifiedName(), outputDirectory);
    }

}
