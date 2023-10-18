package eu.stamp_project.dspot.common.report.output;

import eu.stamp_project.dspot.common.report.Report;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtType;

import java.util.List;

public interface ModificationReport extends Report {

    /**
     * Report a modification made to a test case during the amplification.
     *
     * @param testClass                  fully qualified name of the class containing the tests
     * @param testNameBeforeModification simple name of the test before the amplifier was applied
     * @param testNameAfterModification  simple name of the test case after modification (with amplifier suffix)
     * @param report                     the {@link AmplifierReport} that represents the modification
     */
    public void reportModification(CtType<?> testClass, String testNameBeforeModification,
                                   String testNameAfterModification, AmplifierReport report);

    /**
     * Filter the report to only keep modifications made to the test cases given in
     *
     * @param testClass     fully qualified name of the class containing the tests
     * @param selectedTests tests of which to keep the reports
     */
    public void filterModifications(CtType<?> testClass, List<CtMethod<?>> selectedTests);

}
