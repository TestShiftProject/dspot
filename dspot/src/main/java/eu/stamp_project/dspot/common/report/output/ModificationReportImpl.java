package eu.stamp_project.dspot.common.report.output;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtType;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Keeps track of the modifications made to each test case during the amplification process.
 */
public class ModificationReportImpl implements ModificationReport {

    private static final Logger LOGGER = LoggerFactory.getLogger(ModificationReportImpl.class);

    /**
     * Maps the qualified name of the class to its report.
     */
    private Map<String, ClassModificationReport> reportsPerClass;

    public ModificationReportImpl() {
        this.reportsPerClass = new HashMap<>();
    }

    @Override
    public void output(String outputDirectory) {
        this.reportsPerClass.entrySet().stream().filter(entry -> entry.getValue() != null)
                .forEach(entry -> entry.getValue().output(entry.getKey(), outputDirectory));

        LOGGER.info("Modification report was output to {}", outputDirectory);
        LOGGER.info("Modification report was: {}", reportsPerClass.toString());
    }

    @Override
    public void outputForClass(String outputDirectory, CtType<?> testClass) {
        this.reportsPerClass.entrySet().stream()
                .filter(entry -> entry.getKey().equals(testClass.getQualifiedName()) && entry.getValue() != null)
                .forEach(entry -> entry.getValue().output(entry.getKey(), outputDirectory));

        LOGGER.info("Modification report of class {} was output to {}", testClass.getQualifiedName(), outputDirectory);
    }

    @Override
    public void reset() {
        reportsPerClass.clear();
    }

    @Override
    public void reportModification(CtType<?> testClass, String testNameBeforeModification,
                                   String testNameAfterModification, AmplifierReport report) {
        String testClassName = "null_class_name";
        if (testClass != null) {
            testClassName = testClass.getQualifiedName();
        }

        ClassModificationReport classModificationReport = reportsPerClass.get(testClassName);
        if (classModificationReport == null) {
            classModificationReport = new ClassModificationReport();
            reportsPerClass.put(testClassName, classModificationReport);
        }
        classModificationReport.reportModification(testNameBeforeModification, testNameAfterModification, report);
    }

    @Override
    public void filterModifications(CtType<?> testClass, List<CtMethod<?>> selectedTests) {
        ClassModificationReport classModificationReport = reportsPerClass.get(testClass.getQualifiedName());
        if (classModificationReport != null) {
            classModificationReport.filterModifications(selectedTests);
        }
    }
}
