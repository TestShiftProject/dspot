package eu.stamp_project.dspot.common.report.output;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.reflect.declaration.CtType;

import java.util.ArrayList;
import java.util.List;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 09/04/19
 */
public class OutputReportImpl implements OutputReport {

    private static final Logger LOGGER = LoggerFactory.getLogger(OutputReport.class);

    private final List<String> linesOfOutputtingAmplifiedTestClasses;

    private int totalNumberOfAmplifiedTestMethods;
    private int totalNumberOfAmplifiedTestClasses;

    public OutputReportImpl() {
        this.linesOfOutputtingAmplifiedTestClasses = new ArrayList<>();
        this.totalNumberOfAmplifiedTestMethods = 0;
        this.totalNumberOfAmplifiedTestClasses = 0;
    }

    @Override
    public void addNumberAmplifiedTestMethodsToTotal(int numberOfAmplifiedTestMethods) {
        this.totalNumberOfAmplifiedTestMethods += numberOfAmplifiedTestMethods;
    }

    @Override
    public void addPrintedTestClasses(String line, boolean printedAmplifiedTestClass) {
        this.linesOfOutputtingAmplifiedTestClasses.add(line);
        if (printedAmplifiedTestClass) {
            totalNumberOfAmplifiedTestClasses++;
        }
    }

    @Override
    public void output(String outputDirectory) {
        LOGGER.info("The amplification ends up with {} amplified test methods over {} test classes.",
                this.totalNumberOfAmplifiedTestMethods,
                this.totalNumberOfAmplifiedTestClasses
        );
        this.linesOfOutputtingAmplifiedTestClasses.forEach(LOGGER::info);
    }

    @Override
    public void outputForClass(String outputDirectory, CtType<?> testClass) {
        LOGGER.info("Finished amplification of {}. The amplification currently selected {} amplified test methods " +
                        "over {} test classes.",
                testClass.getQualifiedName(),
                this.totalNumberOfAmplifiedTestMethods,
                this.totalNumberOfAmplifiedTestClasses
        );
    }

    @Override
    public void reset() {
        this.linesOfOutputtingAmplifiedTestClasses.clear();
        this.totalNumberOfAmplifiedTestMethods = 0;
        this.totalNumberOfAmplifiedTestClasses = 0;
    }
}
