package eu.stamp_project.prettifier;

import eu.stamp_project.dspot.common.test_framework.TestFramework;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.Launcher;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.factory.Factory;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 04/02/19
 */
public class RedundantCastRemoverTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(RedundantCastRemoverTest.class);

    @Test
    public void test() {

        /*
            This tests that we can remove some redundant cast.

            The last statement should keep its cast
         */

        Launcher launcher = new Launcher();
        launcher.addInputResource("src/test/java/eu/stamp_project/resources/AmplifiedTest.java");
        launcher.buildModel();

        TestFramework.init(launcher.getFactory());

        final Factory factory = launcher.getFactory();
        final CtMethod<?> redundantCast = factory.Class().get("eu.stamp_project.resources.AmplifiedTest")
                .getMethodsByName("redundantCast").get(0);

        final RedundantCastRemover redundantCastRemover = new RedundantCastRemover();
        final CtMethod<?> amplifiedTestWithoutRedundantCast = redundantCastRemover.remove(redundantCast);
        Assertions.assertEquals(expected, amplifiedTestWithoutRedundantCast.getBody().toString());
    }

    private static final String expected = "{\n" +
            "    final eu.stamp_project.resources.AmplifiedTest amplifiedTest = new eu.stamp_project.resources.AmplifiedTest();\n" +
            "    final eu.stamp_project.resources.AmplifiedTest.MyObject myObject = new eu.stamp_project.resources.AmplifiedTest.MyObject();\n" +
            "    // should be removed\n" +
            "    org.junit.Assert.assertEquals(0, amplifiedTest.getInt());\n" +
            "    org.junit.Assert.assertEquals(0, amplifiedTest.getInt());\n" +
            "    org.junit.Assert.assertEquals(0, myObject.getInt());\n" +
            "    org.junit.Assert.assertEquals(0, myObject.getMyInternalObject().getInt());\n" +
            "    org.junit.Assert.assertEquals(0, myObject.getMyInternalObject().getMySecondIntegernalObject().getInt());\n" +
            "    org.junit.Assert.assertEquals(0, myObject.getMyInternalObject().getMySecondIntegernalObject().getInt());\n" +
            "    // should not be removed\n" +
            "    org.junit.Assert.assertEquals(0, ((eu.stamp_project.resources.AmplifiedTest.MySecondInternalObject) (myObject.getMyInternalObject().getMySecondIntegernalObject().getObject())).getSecondInt());\n" +
            "}";
}
