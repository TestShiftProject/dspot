package eu.stamp_project.prettifier.configuration;

import eu.stamp_project.dspot.common.miscellaneous.DSpotUtils;
import eu.stamp_project.dspot.common.report.error.Error;
import eu.stamp_project.dspot.common.report.error.ErrorEnum;
import picocli.CommandLine;

import java.io.File;

import static eu.stamp_project.dspot.common.configuration.DSpotState.GLOBAL_REPORT;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 11/02/19
 */
@CommandLine.Command(name = "eu.stamp_project.prettifier.Main", mixinStandardHelpOptions = true)
public class UserInput extends eu.stamp_project.dspot.common.configuration.UserInput {

    public UserInput() {

    }

    @CommandLine.Option(
            names = "--path-to-amplified-test-class",
            description = "Specify the path to the java test class that has been amplified " +
                    "and that contains some amplified test methods to be \"prettified\"." +
                    "If the test class is not in the package with the standard test classes, you need to set " +
                    "this parameter!"
    )
    private String pathToAmplifiedTestClass = "";

    public String getPathToAmplifiedTestClass() {
        return this.pathToAmplifiedTestClass;
    }

    public UserInput setPathToAmplifiedTestClass(String pathToAmplifiedTestClass) {
        if (!pathToAmplifiedTestClass.endsWith(".java")) {
            GLOBAL_REPORT.addInputError(new Error(ErrorEnum.ERROR_PATH_TO_AMPLIFIED_TEST_FILE));
            return this;
        }
        this.pathToAmplifiedTestClass = pathToAmplifiedTestClass;
        return this;
    }

    @CommandLine.Option(
            names = "--path-to-dspot-reports",
            description = "Specify the path to the reporting jsons provided by DSpot." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "target/dspot/output"
    )
    private String pathToDSpotReports = "target" + File.separator + "dspot" + File.separator + "output";

    public String getPathToDSpotReports() {
        return this.pathToDSpotReports;
    }

    public UserInput setPathToDSpotReports(String pathToDSpotReports) {
        this.pathToDSpotReports = pathToDSpotReports;
        return this;
    }

    // which Prettifiers to apply

    @CommandLine.Option(
            names = "--apply-all-prettifiers",
            description = "Apply all available prettifiers. This overrides options that turn off specific prettifiers." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "false"
    )
    private boolean applyAllPrettifiers;

    public boolean isApplyAllPrettifiers() {
        return applyAllPrettifiers;
    }

    public UserInput setApplyAllPrettifiers(boolean applyAllPrettifiers) {
        this.applyAllPrettifiers = applyAllPrettifiers;
        return this;
    }

    @CommandLine.Option(
            names = "--apply-general-minimizer",
            description = "Apply the general minimizer to remove redundant assertions and inline local variables." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "false"
    )
    private boolean applyGeneralMinimizer;

    public boolean isApplyGeneralMinimizer() {
        return applyGeneralMinimizer;
    }

    public UserInput setApplyGeneralMinimizer(boolean applyGeneralMinimizer) {
        this.applyGeneralMinimizer = applyGeneralMinimizer;
        return this;
    }

    @CommandLine.Option(
            names = "--apply-pit-minimizer",
            description = "Apply the pit minimizer to remove assertions that do not improve the mutation score." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "false"
    )
    private boolean applyPitMinimizer;

    public boolean isApplyPitMinimizer() {
        return applyPitMinimizer;
    }

    public UserInput setApplyPitMinimizer(boolean applyPitMinimizer) {
        this.applyPitMinimizer = applyPitMinimizer;
        return this;
    }

    @CommandLine.Option(
            names = "--apply-extended-coverage-minimizer",
            description = "Apply the extended coverage minimizer to remove statements that do not affect the " +
                    "contributed coverage." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "false"
    )
    private boolean applyExtendedCoverageMinimizer;

    public boolean isApplyExtendedCoverageMinimizer() {
        return applyExtendedCoverageMinimizer;
    }

    public UserInput setApplyExtendedCoverageMinimizer(boolean applyExtendedCoverageMinimizer) {
        this.applyExtendedCoverageMinimizer = applyExtendedCoverageMinimizer;
        return this;
    }

    @CommandLine.Option(
            names = "--filter-dev-friendly",
            description = "Filter the test cases according to which tests developers find useful." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "false"
    )
    private boolean filterDevFriendly;

    public boolean isFilterDevFriendly() {
        return filterDevFriendly;
    }

    public UserInput setFilterDevFriendly(boolean filterDevFriendly) {
        this.filterDevFriendly = filterDevFriendly;
        return this;
    }

    @CommandLine.Option(
            names = "--prioritize-most-coverage",
            description = "Filter the test cases according to which tests developers find useful." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "false"
    )
    private boolean prioritizeMostCoverage;

    public boolean isPrioritizeMostCoverage() {
        return prioritizeMostCoverage;
    }

    public UserInput setPrioritizeMostCoverage(boolean prioritizeMostCoverage) {
        this.prioritizeMostCoverage = prioritizeMostCoverage;
        return this;
    }

    @CommandLine.Option(
            names = "--rename-test-methods",
            description = "Choose a TestRenamer to give the tests new names." +
                    " Valid values: ${COMPLETION-CANDIDATES}" +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "None"
    )
    private TestRenamerEnum testRenamer;

    public TestRenamerEnum getTestRenamer() {
        return testRenamer;
    }

    public UserInput setTestRenamer(TestRenamerEnum testRenamer) {
        this.testRenamer = testRenamer;
        return this;
    }

    @CommandLine.Option(
            names = "--rename-local-variables",
            description = "Choose a VariableRenamer to give local variables new names." +
                    " Valid values: ${COMPLETION-CANDIDATES}" +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "None"
    )
    private VariableRenamerEnum variableRenamer;

    public VariableRenamerEnum getVariableRenamer() {
        return variableRenamer;
    }

    public UserInput setVariableRenamer(VariableRenamerEnum variableRenamer) {
        this.variableRenamer = variableRenamer;
        return this;
    }

    @CommandLine.Option(
            names = "--generate-descriptions",
            description = "Generate textual descriptions of the test case's contributions." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "false"
    )
    private boolean generateTestDescriptions;

    public boolean isGenerateTestDescriptions() {
        return generateTestDescriptions;
    }

    public UserInput setGenerateTestDescriptions(boolean generateTestDescriptions) {
        this.generateTestDescriptions = generateTestDescriptions;
        return this;
    }

    @CommandLine.Option(
            names = "--remove-redundant-casts",
            description = "Remove redundant casts in the generated assertions." +
                    " Default value: ${DEFAULT-VALUE}",
            defaultValue = "false"
    )
    private boolean removeRedundantCasts;

    public boolean isRemoveRedundantCasts() {
        return removeRedundantCasts;
    }

    public UserInput setRemoveRedundantCasts(boolean removeRedundantCasts) {
        this.removeRedundantCasts = removeRedundantCasts;
        return this;
    }

    // Code2Vec

    @CommandLine.Option(
            names = "--path-to-code2vec",
            description = "[mandatory] Specify the path to the folder root of Code2Vec. " +
                    "This folder should be a fresh clone of https://github.com/tech-srl/code2vec.git" +
                    "We advise you to use absolute path."
    )
    private String pathToRootOfCode2Vec;

    public String getPathToRootOfCode2Vec() {
        return pathToRootOfCode2Vec;
    }

    public UserInput setPathToRootOfCode2Vec(String pathToRootOfCode2Vec) {
        this.pathToRootOfCode2Vec = DSpotUtils.shouldAddSeparator.apply(pathToRootOfCode2Vec);
        return this;
    }

    @CommandLine.Option(
            names = "--path-to-code2vec-model",
            description = "[mandatory] Specify the relative path to the model trained with Code2Vec. " +
                    "This path will be use relatively from --path-to-code2vec value."
    )
    private String relativePathToModelForCode2Vec;

    public String getRelativePathToModelForCode2Vec() {
        return relativePathToModelForCode2Vec;
    }

    public UserInput setRelativePathToModelForCode2Vec(String relativePathToModelForCode2Vec) {
        this.relativePathToModelForCode2Vec = relativePathToModelForCode2Vec;
        return this;
    }

    private long timeToWaitForCode2vecInMillis = 90000;

    public long getTimeToWaitForCode2vecInMillis() {
        return this.timeToWaitForCode2vecInMillis;
    }

    public UserInput setTimeToWaitForCode2vecInMillis(long timeToWaitForCode2vecInMillis) {
        this.timeToWaitForCode2vecInMillis = timeToWaitForCode2vecInMillis;
        return this;
    }

    // Context2Name

    private String pathToRootOfContext2Name;

    public String getPathToRootOfContext2Name() {
        return pathToRootOfContext2Name;
    }

    public UserInput setPathToRootOfContext2Name(String pathToRootOfContext2Name) {
        this.pathToRootOfContext2Name = DSpotUtils.shouldAddSeparator.apply(pathToRootOfContext2Name);
        return this;
    }

    private String relativePathToModelForContext2Name;

    public String getRelativePathToModelForContext2Name() {
        return relativePathToModelForContext2Name;
    }

    public UserInput setRelativePathToModelForContext2Name(String relativePathToModelForContext2Name) {
        this.relativePathToModelForContext2Name = relativePathToModelForContext2Name;
        return this;
    }

    private long timeToWaitForContext2nameInMillis = 90000;

    public long getTimeToWaitForContext2nameInMillis() {
        return this.timeToWaitForContext2nameInMillis;
    }

    public UserInput setTimeToWaitForContext2nameInMillis(long timeToWaitForContext2nameInMillis) {
        this.timeToWaitForContext2nameInMillis = timeToWaitForContext2nameInMillis;
        return this;
    }
}
