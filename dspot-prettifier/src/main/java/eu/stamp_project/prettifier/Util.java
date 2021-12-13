package eu.stamp_project.prettifier;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.typeadapters.RuntimeTypeAdapterFactory;
import eu.stamp_project.dspot.common.report.output.AmplifierReport;
import eu.stamp_project.dspot.common.report.output.ClassModificationReport;
import eu.stamp_project.dspot.common.report.output.amplifiers.AddLocalVariableAmplifierReport;
import eu.stamp_project.dspot.common.report.output.amplifiers.MethodAdderOnExistingObjectsAmplifierReport;
import eu.stamp_project.dspot.common.report.output.amplifiers.ValueAmplifierReport;
import eu.stamp_project.dspot.common.report.output.assertiongenerator.ValueAssertionReport;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestClassJSON;
import eu.stamp_project.dspot.selector.extendedcoverageselector.CoverageImprovement;
import eu.stamp_project.dspot.selector.extendedcoverageselector.MethodCoverage;
import eu.stamp_project.prettifier.configuration.UserInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.reflect.code.CtLocalVariable;
import spoon.reflect.code.CtVariableRead;
import spoon.reflect.reference.CtLocalVariableReference;
import spoon.reflect.visitor.filter.TypeFilter;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Util {

    private static final Logger LOGGER = LoggerFactory.getLogger(Util.class);

    public static final class LOCAL_VARIABLE_READ_FILTER extends TypeFilter<CtVariableRead<?>> {
        private final CtLocalVariableReference<?> localVariableReference;

        public LOCAL_VARIABLE_READ_FILTER(CtLocalVariable<?> localVariable) {
            super(CtVariableRead.class);
            this.localVariableReference = localVariable.getReference();
        }

        @Override
        public boolean matches(CtVariableRead element) {
            return localVariableReference.equals(element.getVariable());
        }
    }

    public static TestClassJSON readExtendedCoverageResultJSON(UserInput configuration) {
        return (TestClassJSON) readReportJSON(configuration, "", TestClassJSON.class);
    }

    public static ClassModificationReport readModificationReport(UserInput configuration) {
        return (ClassModificationReport) readReportJSON(configuration, "_modification", ClassModificationReport.class);
    }

    private static Object readReportJSON(UserInput configuration, String specifier, Class<?> targetClass) {
        RuntimeTypeAdapterFactory<AmplifierReport> runtimeTypeAdapterFactory = RuntimeTypeAdapterFactory
                .of(AmplifierReport.class, "reportType")
                .registerSubtype(ValueAssertionReport.class, ValueAssertionReport.class.getCanonicalName())
                .registerSubtype(MethodAdderOnExistingObjectsAmplifierReport.class, MethodAdderOnExistingObjectsAmplifierReport.class.getCanonicalName())
                .registerSubtype(ValueAmplifierReport.class, ValueAmplifierReport.class.getCanonicalName())
                .registerSubtype(AddLocalVariableAmplifierReport.class,
                        AddLocalVariableAmplifierReport.class.getCanonicalName());
        Gson gson = new GsonBuilder().registerTypeAdapterFactory(runtimeTypeAdapterFactory).create();
        try {
            return gson.fromJson(new FileReader(configuration.getPathToDSpotReports() + File.separator
                            + configuration.getTestClasses().get(0) + specifier + "_report.json"),
                    targetClass);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void writeReportJSON(UserInput configuration, Object json, String specifier) {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        final String pathname = configuration.getOutputDirectory() + File.separator
                + configuration.getTestClasses().get(0) + specifier + "_report.json";
        LOGGER.info("Output {} report in {}", specifier, pathname);
        final File file = new File(pathname);
        try (FileWriter writer = new FileWriter(file, false)) {
            writer.write(gson.toJson(json));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * @param coverageImprovement of the test case
     * @return a list of all method names where this test case improves coverage
     */
    public static List<String> getCoveredMethods(CoverageImprovement coverageImprovement) {
        List<String> methodNames = new ArrayList<>();
        coverageImprovement.getInstructionImprovement().classCoverageMaps.forEach((className, classCoverageMap) -> {
            List<Map.Entry<String, MethodCoverage>> methodNamesAndMethodCoverages =
                    new ArrayList<>(classCoverageMap.methodCoverageMap.entrySet());

            // put the methods with most additional coverage first (to be first in the name later)
            methodNamesAndMethodCoverages.sort((e1, e2) ->
                    Integer.compare(e1.getValue().totalAdditionallyCoveredInstructions(),
                            e2.getValue().totalAdditionallyCoveredInstructions())
            );

            methodNamesAndMethodCoverages.forEach((entry) -> {
                methodNames.add(entry.getKey());
            });
        });
        return methodNames;
    }
}