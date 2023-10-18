package eu.stamp_project.dspot.selector.extendedcoverageselector;

import java.util.*;
import java.util.stream.Collectors;

public class ClassCoverageMap {

    public ClassCoverageMap() {
        methodCoverageMap = new HashMap<>();
    }

    /**
     * Maps the simple name of a method to its coverage.
     */
    public Map<String, MethodCoverage> methodCoverageMap;

    public MethodCoverage getCoverageForMethod(String methodName) {
        return methodCoverageMap.get(methodName);
    }

    public void addMethodCoverage(String methodName, MethodCoverage methodCoverage) {
        methodCoverageMap.put(methodName, methodCoverage);
    }

    public ClassCoverageMap improvementDiffOver(ClassCoverageMap that) {
        ClassCoverageMap thizBetterDiff = new ClassCoverageMap();
        for (Map.Entry<String, MethodCoverage> entry : this.methodCoverageMap.entrySet()) {
            MethodCoverage coverageThat = that.getCoverageForMethod(entry.getKey());
            if (coverageThat == null) {
                thizBetterDiff.addMethodCoverage(entry.getKey(), entry.getValue());
            } else {
                MethodCoverage improvementDiff = entry.getValue().improvementDiffOver(coverageThat);
                if (improvementDiff.lineCoverage.parallelStream().anyMatch(i -> i > 0)) {
                    thizBetterDiff.addMethodCoverage(entry.getKey(), improvementDiff);
                }
            }

        }
        return thizBetterDiff;
    }

    public ClassCoverageMap accumulate(ClassCoverageMap toAdd) {
        Set<String> mergedKeys = new HashSet<>();
        mergedKeys.addAll(this.methodCoverageMap.keySet());
        mergedKeys.addAll(toAdd.methodCoverageMap.keySet());

        ClassCoverageMap accumulated = new ClassCoverageMap();
        for (String mergedKey : mergedKeys) {
            MethodCoverage valuesBase = this.methodCoverageMap.get(mergedKey);
            MethodCoverage valuesToAdd = toAdd.methodCoverageMap.get(mergedKey);

            if (valuesBase == null) {
                accumulated.addMethodCoverage(mergedKey, valuesToAdd);
            } else if (valuesToAdd == null) {
                accumulated.addMethodCoverage(mergedKey, valuesBase);
            } else {
                accumulated.addMethodCoverage(mergedKey, valuesBase.accumulate(valuesToAdd));
            }
        }
        return accumulated;

    }

    /**
     * @return the total number of covered instructions.
     */
    public int sum() {
        return methodCoverageMap.values().stream().map(MethodCoverage::sum).mapToInt(Integer::intValue).sum();
    }


    /**
     * Returns the method coverages for all methods whose name matches the provided regex.
     *
     * @param methodNameRegex matched on the simple name of each covered
     * @return all method coverages provided in this project coverage, where the name matches the provided regex.
     */
    public List<MethodCoverage> getCoverageForMethodsMatching(String methodNameRegex) {
        return methodCoverageMap.entrySet().stream()
                .filter(entry -> entry.getKey().matches(methodNameRegex))
                .map(Map.Entry::getValue).collect(Collectors.toList());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ClassCoverageMap that = (ClassCoverageMap) o;
        return Objects.equals(methodCoverageMap, that.methodCoverageMap);
    }

    @Override
    public int hashCode() {
        return Objects.hash(methodCoverageMap);
    }
}

