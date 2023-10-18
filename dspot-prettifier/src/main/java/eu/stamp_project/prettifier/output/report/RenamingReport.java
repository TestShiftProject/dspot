package eu.stamp_project.prettifier.output.report;

import spoon.reflect.declaration.CtMethod;

import java.util.*;

public class RenamingReport {

    /**
     * Maps 'testName#oldVariableName' to 'newVariableName'
     */
    private final Map<String, String> renamedVariables;


    /**
     * Maps 'testName#oldExpression' to 'newExpression'
     */
    private final Map<String, String> reducedCasts;

    /**
     * Maps the old test name to the new test name.
     */
    private final Map<String, String> renamedTests;

    public RenamingReport() {
        renamedVariables = new HashMap<>();
        reducedCasts = new HashMap<>();
        renamedTests = new HashMap<>();
    }

    /**
     * Reports the renaming of a variable within a test.
     *
     * @param test    the test containing the variable.
     * @param oldName the old name of the variable.
     * @param newName the new name of the variable.
     */
    public void addVariableRenaming(CtMethod<?> test, String oldName, String newName) {
        renamedVariables.put(test.getSimpleName() + "#" + oldName, newName);
    }

    public Optional<String> getNewVariableName(CtMethod<?> test, String oldName) {
        return Optional.ofNullable(renamedVariables.get(test.getSimpleName() + "#" + oldName));
    }

    public Map<String, String> getRenamedVariables() {
        return renamedVariables;
    }

    /**
     * Reports the reduction of a cast within a test.
     *
     * @param test          the test containing the cast.
     * @param oldExpression the old String representation of the expression (with casts).
     * @param newExpression the new String representation of the expression (without casts).
     */
    public void addCastReduction(CtMethod<?> test, String oldExpression, String newExpression) {
        if (oldExpression.equals(newExpression)) {
            // no need to save this, nothing changed actually :)
            return;
        }
        reducedCasts.put(test.getSimpleName() + "#" + oldExpression, newExpression);
    }

    public Optional<String> getCastAfterReduction(CtMethod<?> test, String oldExpression) {
        return Optional.ofNullable(reducedCasts.get(test.getSimpleName() + "#" + oldExpression));
    }

    public Map<String, String> getReducedCasts() {
        return reducedCasts;
    }


    /**
     * Reports the renaming of a test method
     *
     * @param test    the amplified test case *before* renaming.
     * @param newName the original test case name.
     */
    public void addTestRenaming(CtMethod<?> test, String newName) {
        renamedTests.put(test.getSimpleName(), newName);
    }

    public Optional<String> getNewTestName(String oldName) {
        return Optional.ofNullable(renamedTests.get(oldName));
    }

    /**
     * Should be called after all test renamings are recorded.
     * Updates all other renaming reports (that identify a renaming by the test name) to use the new test names.
     */
    public void updateOtherReportsAfterTestRenaming() {
        for (Map.Entry<String, String> testRenaming : renamedTests.entrySet()) {
            updateReportToNewName(reducedCasts, testRenaming.getKey(), testRenaming.getValue());
            updateReportToNewName(renamedVariables, testRenaming.getKey(), testRenaming.getValue());
        }
    }

    private void updateReportToNewName(Map<String, String> report, String oldName, String newName) {
        List<String> outdatedReportEntries = new ArrayList<>();
        Map<String, String> newReportEntries = new HashMap<>();

        for (Map.Entry<String, String> reportEntry : report.entrySet()) {
            String[] keyParts = reportEntry.getKey().split("#");
            if (keyParts[0].equals(oldName)) {
                outdatedReportEntries.add(reportEntry.getKey());
                newReportEntries.put(newName + "#" + keyParts[1], reportEntry.getValue());
            }
        }

        for (String outdatedReportEntry : outdatedReportEntries) {
            report.remove(outdatedReportEntry);
        }
        report.putAll(newReportEntries);
    }
}
