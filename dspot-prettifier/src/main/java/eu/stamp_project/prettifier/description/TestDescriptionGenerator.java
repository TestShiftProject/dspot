package eu.stamp_project.prettifier.description;

import eu.stamp_project.dspot.common.configuration.options.CommentEnum;
import eu.stamp_project.dspot.common.miscellaneous.DSpotUtils;
import eu.stamp_project.dspot.common.report.output.AmplifierReport;
import eu.stamp_project.dspot.common.report.output.ClassModificationReport;
import eu.stamp_project.dspot.common.report.output.amplifiers.*;
import eu.stamp_project.dspot.common.report.output.assertiongenerator.ExceptionAssertionReport;
import eu.stamp_project.dspot.common.report.output.assertiongenerator.ValueAssertionReport;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestCaseJSON;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestClassJSON;
import eu.stamp_project.prettifier.Main;
import eu.stamp_project.prettifier.Prettifier;
import eu.stamp_project.prettifier.Util;
import eu.stamp_project.prettifier.configuration.UserInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.reflect.code.CtComment;
import spoon.reflect.declaration.CtMethod;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;

/**
 * Generates a textual description of the contribution that the passed test cases make.
 * The description is added as a javadoc comment and saved in the report.
 */
public class TestDescriptionGenerator implements Prettifier {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestDescriptionGenerator.class);

    final UserInput configuration;

    /**
     * Maps the variables newly defined by DSpot in the test case to their values.
     * As (most? all?) are inlined by the general minimizer, the names can be generally replaced by the value in the
     * descriptions.
     * Is cleared before describing a new test case.
     */
    private final Map<String, String> variableValues;


    public TestDescriptionGenerator(UserInput configuration) {
        this.configuration = configuration;
        variableValues = new HashMap<>();
    }

    @Override
    public List<CtMethod<?>> prettify(List<CtMethod<?>> amplifiedTestsToBePrettified) {
        List<CtMethod<?>> prettifiedTests = new ArrayList<>();

        boolean coverageReportPresent = Main.report.isExtendedCoverageReportPresent(this.getClass().getSimpleName());
        if (!coverageReportPresent) {
            return amplifiedTestsToBePrettified;
        }
        TestClassJSON amplificationReport = Main.report.extendedCoverageReport;
        Map<String, TestCaseJSON> mapTestNameToResult = amplificationReport.mapTestNameToResult();

        boolean modificationReportPresent = Main.report.isModificationReportPresent(this.getClass().getSimpleName());
        if (!modificationReportPresent) {
            return amplifiedTestsToBePrettified;
        }
        ClassModificationReport modificationReport = Main.report.modificationReport;

        for (CtMethod<?> test : amplifiedTestsToBePrettified) {
            prettifiedTests.add(prettify(test, modificationReport.getModificationsForTest(test),
                    mapTestNameToResult.get(test.getSimpleName())));
        }

        return prettifiedTests;
    }

    private CtMethod<?> prettify(CtMethod<?> amplifiedTest, List<AmplifierReport> modificationsForTest,
                                 TestCaseJSON testCaseResult) {
        variableValues.clear();
        // Map modifications per type
        Map<String, List<AmplifierReport>> modifications =
                modificationsForTest.stream().collect(Collectors.groupingBy(AmplifierReport::getReportType));

        for (AmplifierReport localVariableReport :
                modifications.getOrDefault(AddLocalVariableAmplifierReport.class.getCanonicalName(), emptyList())) {
            rememberLocalVariable((AddLocalVariableAmplifierReport) localVariableReport);
        }

        StringBuilder description = new StringBuilder("Test that ");

        // Test that
        // x is y
        addAssertionText(description, amplifiedTest, modifications);
        // , when a=3 and b=5 .
        addChangeText(description, amplifiedTest, modifications);
        // This tests the methods ...
        addCoverageText(description, amplifiedTest, testCaseResult);
        // The test is based on ...
        addOriginalTestText(description, amplifiedTest, testCaseResult);

        String testDescription = replaceRenamedTextsIfPresent(description.toString(), amplifiedTest);

        DSpotUtils.addComment(amplifiedTest, testDescription, CtComment.CommentType.JAVADOC, CommentEnum.All);
        Main.report.extendedCoverageReport.updateTestCase(testCaseResult,
                testCaseResult.copyAndUpdateDescription(testDescription));
        return amplifiedTest;
    }

    /**
     * Adds a text for the assertion in the form of "`testedExpression` is expectedValue"
     *
     * @param description   the string builder holding the description
     * @param test          the test to be described
     * @param modifications the modifications made to the test during the amplification
     */
    private void addAssertionText(StringBuilder description, CtMethod<?> test, Map<String, List<AmplifierReport>> modifications) {
        List<AmplifierReport> valueAssertionReports =
                modifications.getOrDefault(ValueAssertionReport.class.getCanonicalName(), emptyList());

        if (valueAssertionReports.isEmpty()) {
            List<AmplifierReport> exceptionAssertionReports =
                    modifications.getOrDefault(ExceptionAssertionReport.class.getCanonicalName(), emptyList());

            if (exceptionAssertionReports.isEmpty()) {
                LOGGER.warn("Tried to generate description for test without any reported assertion modifications, no " +
                        "assertion text added.");
            } else {
                for (AmplifierReport report : exceptionAssertionReports) {
                    ExceptionAssertionReport exceptionAssertionReport = (ExceptionAssertionReport) report;
                    description.append("a ");
                    addCodeText(description, exceptionAssertionReport.getExceptionName());
                    description.append(" and ");
                }
                replaceEndIfThere(description, " and ", "");
                description.append(" is thrown");
            }
        } else {
            for (AmplifierReport report : valueAssertionReports) {
                ValueAssertionReport valueAssertionReport = (ValueAssertionReport) report;

                // TODO: move adding only "testedValue" to the top here again when it is assigned properly for
                //  single-parameter assertions
                switch (valueAssertionReport.getAssertionType()) {
                    case ASSERT_NULL:
                        addCodeText(description, replaceLocalVariableIfPresent(valueAssertionReport.getExpectedValue(),
                                test));
                        description.append(" is null");
                        break;
                    case ASSERT_NOT_NULL:
                        addCodeText(description, replaceLocalVariableIfPresent(valueAssertionReport.getExpectedValue(),
                                test));
                        description.append(" is not null");
                        break;
                    case ASSERT_TRUE:
                        addCodeText(description, replaceLocalVariableIfPresent(valueAssertionReport.getExpectedValue(),
                                test));
                        description.append(" is true");
                        break;
                    case ASSERT_FALSE:
                        addCodeText(description, replaceLocalVariableIfPresent(valueAssertionReport.getExpectedValue(),
                                test));
                        description.append(" is false");
                        break;
                    case ASSERT_EQUALS:
                        addCodeText(description, replaceLocalVariableIfPresent(valueAssertionReport.getTestedValue(),
                                test));
                        description.append(" is equal to ");
                        addCodeText(description, valueAssertionReport.getExpectedValue());
                        break;
                    case ASSERT_NOT_EQUALS:
                        addCodeText(description, replaceLocalVariableIfPresent(valueAssertionReport.getTestedValue(),
                                test));
                        description.append(" is not equal to ");
                        addCodeText(description, valueAssertionReport.getExpectedValue());
                        break;
                    case ASSERT_ARRAY_EQUALS:
                        addCodeText(description, replaceLocalVariableIfPresent(valueAssertionReport.getTestedValue(),
                                test));
                        description.append(" is equal to the array ");
                        addCodeText(description, valueAssertionReport.getExpectedValue());
                        break;
                    default:
                        break;
                }

                description.append(" and ");
            }
            replaceEndIfThere(description, " and ", "");
        }
    }

    private void addChangeText(StringBuilder description, CtMethod<?> test, Map<String, List<AmplifierReport>> modifications) {
        description.append(" when ");

        // NOTE: Insert " and " in between these when there is more than one large modification in the amplified test!
        // (e.g. when not using the developer-centric amplification in DSpot)

        addLiteralChangedText(description, modifications.getOrDefault(LiteralAmplifierReport.class.getCanonicalName(),
                emptyList()), test);

        List<AmplifierReport> methodAdderReports =
                modifications.getOrDefault(MethodDuplicationAmplifierReport.class.getCanonicalName(), new ArrayList<>());
        methodAdderReports.addAll(modifications.getOrDefault(MethodAdderOnExistingObjectsAmplifierReport.class.getCanonicalName(), emptyList()));
        addMethodCalledOrDuplicationText(description, methodAdderReports, test);

        addMethodCallRemovedText(description,
                modifications.getOrDefault(MethodRemoveAmplifierReport.class.getCanonicalName(), emptyList()));
        addLocalVariableText(description,
                modifications.getOrDefault(AddLocalVariableAmplifierReport.class.getCanonicalName(), emptyList()),
                test);

        replaceEndByPeriodIfThere(description, " when ");
    }

    /**
     * reports for {@link AddLocalVariableAmplifierReport}
     */
    private void addLocalVariableText(StringBuilder description, List<AmplifierReport> modifications,
                                      CtMethod<?> test) {
        for (AmplifierReport amplifierReport : modifications) {
            if (amplifierReport.isAssertionReport()) {
                // local variable used in assertion, so it is not mentioned when describing the change.
                continue;
            }
            if (amplifierReport.getReportType().equals(AddLocalVariableAmplifierReport.class.getCanonicalName())) {
                AddLocalVariableAmplifierReport localVariableAmplifierReport = (AddLocalVariableAmplifierReport) amplifierReport;
                if (description.indexOf(localVariableAmplifierReport.getVariableName()) != -1) {
                    // variable is used in description until now, so we should report its value!
                    description.append(" ");
                    addCodeText(description, localVariableAmplifierReport.getVariableName());
                    description.append(" is ");
                    addCodeText(description, replaceLocalVariableIfPresent(localVariableAmplifierReport.getVariableValue(),
                            test));
                    description.append(" and ");
                }
            }
        }
        replaceEndByPeriodIfThere(description, " and ");
    }

    /**
     * reports for {@link LiteralAmplifierReport}
     */
    private void addLiteralChangedText(StringBuilder description, List<AmplifierReport> modifications,
                                       CtMethod<?> test) {
        for (AmplifierReport amplifierReport : modifications) {
            if (amplifierReport.getReportType().equals(LiteralAmplifierReport.class.getCanonicalName())) {
                LiteralAmplifierReport literalAmplifierReport = (LiteralAmplifierReport) amplifierReport;
                if (literalAmplifierReport.getVariableName().equals("unknown") && literalAmplifierReport.getMethodName().equals("")) {
                    // could not retrieve enough information during the amplification -> better to skip than to
                    // confuse the user
                    continue;
                }

                if (literalAmplifierReport.isLocalVariable()) {
                    description.append(literalAmplifierReport.getVariableName());
                    description.append(" is ");
                    addCodeText(description, replaceLocalVariableIfPresent(literalAmplifierReport.getNewValue(), test));
                } else {
                    // new literal is method call parameter
                    description.append("the parameter ");
                    addCodeText(description, literalAmplifierReport.getVariableName());
                    description.append(" of the method ");
                    addCodeText(description, literalAmplifierReport.getMethodName());
                    description.append(" is set to ");
                    addCodeText(description, replaceLocalVariableIfPresent(literalAmplifierReport.getNewValue(), test));
                }
                description.append(" and ");
            }
        }
        replaceEndByPeriodIfThere(description, " and ");
    }

    /**
     * reports for {@link MethodDuplicationAmplifierReport} and {@link MethodAdderOnExistingObjectsAmplifierReport}
     */
    private void addMethodCalledOrDuplicationText(StringBuilder description, List<AmplifierReport> modifications,
                                                  CtMethod<?> test) {
        for (AmplifierReport amplifierReport : modifications) {
            if (amplifierReport.getReportType().equals(MethodDuplicationAmplifierReport.class.getCanonicalName())) {
                MethodDuplicationAmplifierReport methodDuplicationAmplifierReport = (MethodDuplicationAmplifierReport) amplifierReport;
                addCodeText(description, methodDuplicationAmplifierReport.getDuplicatedCall());
                description.append(" is called");

            } else if (amplifierReport.getReportType().equals(MethodAdderOnExistingObjectsAmplifierReport.class.getCanonicalName())) {
                MethodAdderOnExistingObjectsAmplifierReport methodAdderOnExistingObjectsAmplifierReport =
                        (MethodAdderOnExistingObjectsAmplifierReport) amplifierReport;
                addCodeText(description, methodAdderOnExistingObjectsAmplifierReport.getInvokedMethod().getName());
                description.append(" is called");

                List<MethodAdderOnExistingObjectsAmplifierReport.MethodParameter> parameters =
                        methodAdderOnExistingObjectsAmplifierReport.getInvokedMethod().getParameters();
                if (parameters.isEmpty()) {
                    description.append(" is called");
                } else {
                    description.append(" with the parameter");
                    if (parameters.size() > 1) {
                        description.append("s ");
                    } else {
                        description.append(" ");
                    }
                    for (MethodAdderOnExistingObjectsAmplifierReport.MethodParameter parameter : parameters) {
                        description.append("`");
                        description.append(parameter.getName());
                        description.append(" = ");
                        description.append(replaceLocalVariableIfPresent(parameter.getValue(), test));
                        description.append("`");
                        description.append(" and ");
                    }
                    replaceEndIfThere(description, " and ", "");
                }
            }
            description.append(" and ");
        }
        replaceEndByPeriodIfThere(description, " and ");
    }

    /**
     * reports for {@link MethodRemoveAmplifierReport}
     */
    private void addMethodCallRemovedText(StringBuilder description, List<AmplifierReport> modifications) {
        for (AmplifierReport amplifierReport : modifications) {
            if (amplifierReport.getReportType().equals(MethodRemoveAmplifierReport.class.getCanonicalName())) {
                MethodRemoveAmplifierReport methodRemoveAmplifierReport = (MethodRemoveAmplifierReport) amplifierReport;
                addCodeText(description, methodRemoveAmplifierReport.getRemovedCall());
                description.append(" is not called");
            }
            description.append(" and ");
        }
        replaceEndByPeriodIfThere(description, " and ");
    }


    private void addCoverageText(StringBuilder description, CtMethod<?> test,
                                 TestCaseJSON testCaseResult) {
        List<String> coveredMethods = Util.getCoveredMethodsWithClassName(testCaseResult.getCoverageImprovement());
        description.append(" This tests the method");
        if (coveredMethods.size() > 1) {
            description.append("s ");
        } else {
            description.append(" ");
        }
        for (int i = 0; i < Math.min(coveredMethods.size(), 2); i++) {
            addCodeText(description, coveredMethods.get(i));
            description.append(" and ");
        }
        replaceEndByPeriodIfThere(description, " and ");
    }

    private void addOriginalTestText(StringBuilder description, CtMethod<?> test, TestCaseJSON testCaseResult) {
        description.append(" This test is based on the test ");
        addCodeText(description, testCaseResult.getNameOfBaseTestCase());
        description.append(".");
    }

    private void replaceEndIfThere(StringBuilder description, String textToReplace, String replacement) {
        if (description.subSequence(description.length() - textToReplace.length(), description.length()).equals(textToReplace)) {
            description.replace(description.length() - textToReplace.length(), description.length(), replacement);
        }
    }

    /**
     * replace trailing textToReplace by "."
     */
    private void replaceEndByPeriodIfThere(StringBuilder description, String textToReplace) {
        replaceEndIfThere(description, textToReplace, ".");
    }

    /**
     * Remembers name & value of a created local variable to use in the description later.
     */
    private void rememberLocalVariable(AddLocalVariableAmplifierReport report) {
        variableValues.put(report.getVariableName(), report.getVariableValue());
    }

    /**
     * Replaces any remembered local variables with their value.
     * This is a plain string.replace, no actual parsing of code takes place.
     */
    private String replaceLocalVariableIfPresent(String codeSnippet, CtMethod<?> test) {
        return replaceLocalVariableIfPresent(codeSnippet, test, 4);
    }

    /**
     * Replaces any remembered local variables with their value.
     * This is a plain string.replace, no actual parsing of code takes place.
     * Recursion-supporting version if a variable earlier in the list is used in a replaced expression later in the
     * list.
     */
    private String replaceLocalVariableIfPresent(String codeSnippet, CtMethod<?> test, int recursionStepsLeft) {
        if (codeSnippet == null) {
            return null;
        }
        if (recursionStepsLeft == 0) {
            LOGGER.info("Reached end of recursion steps in replacing local variables with values. Continuing with " +
                    "description that might contain unreplaced variables.");
            return codeSnippet;
        }

        // Before we replace we need to also remove any casts that were also removed by the redundant cast remover
        // (won't be detected anymore by string.contains when we replace the variable name with its value)
        String newSnippet = replaceInDescription(codeSnippet, test, Main.report.renamingReport.getReducedCasts());

        for (String variableName : variableValues.keySet()) {
            if (newSnippet.contains(variableName)) {
                newSnippet = newSnippet.replace(variableName,
                        replaceLocalVariableIfPresent(variableValues.get(variableName), test, recursionStepsLeft - 1));
            }
        }

        return newSnippet;
    }

    /**
     * Appends the codeText to the StringBuilder, surrounded by backticks
     *
     * @param description the StringBuilder
     * @param codeText    the text to insert
     */
    private void addCodeText(StringBuilder description, String codeText) {
        description.append("`").append(codeText).append("`");
    }

    private String replaceRenamedTextsIfPresent(String description, CtMethod<?> test) {
        String replacedCasts =
                replaceInDescription(description, test, Main.report.renamingReport.getReducedCasts());
        return replaceInDescription(replacedCasts, test, Main.report.renamingReport.getRenamedVariables());
    }

    private String replaceInDescription(String description, CtMethod<?> test, Map<String, String> replacementMap) {
        // check if a string that was renamed is present in the description
        for (Map.Entry<String, String> renamingEntry : replacementMap.entrySet()) {
            // testName#oldString
            String[] keyParts = renamingEntry.getKey().split("#");
            if (!test.getSimpleName().equals(keyParts[0])) {
                continue;
            }

            if (description.contains(keyParts[1])) {
                description = description.replace(keyParts[1], renamingEntry.getValue());
            }
        }
        return description;
    }
}
