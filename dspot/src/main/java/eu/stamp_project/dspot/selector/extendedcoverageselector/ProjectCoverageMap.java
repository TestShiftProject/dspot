package eu.stamp_project.dspot.selector.extendedcoverageselector;

import java.util.*;

public class ProjectCoverageMap {

    public ProjectCoverageMap() {
        classCoverageMaps = new HashMap<>();
    }

    public Map<String, ClassCoverageMap> classCoverageMaps;

    public ClassCoverageMap getCoverageForClass(String classNameFQ) {
        return classCoverageMaps.get(classNameFQ);
    }

    public void addClassCoverage(String classNameFQ, ClassCoverageMap classCoverageMap) {
        classCoverageMaps.put(classNameFQ, classCoverageMap);
    }

    public ProjectCoverageMap improvementDiffOver(ProjectCoverageMap that) {

        ProjectCoverageMap thizBetterDiff = new ProjectCoverageMap();
        this.classCoverageMaps.entrySet().stream()
                .filter(entry -> that.classCoverageMaps.containsKey(entry.getKey()))
                .forEach(entry -> {
                    ClassCoverageMap coverageThat = that.getCoverageForClass(entry.getKey());
                    ClassCoverageMap improvementDiff = entry.getValue().improvementDiffOver(coverageThat);
                    if (!improvementDiff.methodCoverageMap.isEmpty()) {
                        thizBetterDiff.addClassCoverage(entry.getKey(), improvementDiff);
                    }
                });
        return thizBetterDiff;
    }

    /**
     * @param toAdd coverage map to be accumulated on top
     * @return shared coverage of this and toAdd
     */
    public ProjectCoverageMap accumulate(ProjectCoverageMap toAdd) {
        Set<String> mergedKeys = new HashSet<>();
        mergedKeys.addAll(this.classCoverageMaps.keySet());
        mergedKeys.addAll(toAdd.classCoverageMaps.keySet());

        ProjectCoverageMap accumulated = new ProjectCoverageMap();
        for (String mergedKey : mergedKeys) {
            ClassCoverageMap valuesBase = this.classCoverageMaps.get(mergedKey);
            ClassCoverageMap valuesToAdd = toAdd.classCoverageMaps.get(mergedKey);

            if (valuesBase == null) {
                accumulated.addClassCoverage(mergedKey, valuesToAdd);
            } else if (valuesToAdd == null) {
                accumulated.addClassCoverage(mergedKey, valuesBase);
            } else {
                accumulated.addClassCoverage(mergedKey, valuesBase.accumulate(valuesToAdd));
            }
        }
        return accumulated;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ProjectCoverageMap that = (ProjectCoverageMap) o;
        return Objects.equals(classCoverageMaps, that.classCoverageMaps);
    }

    @Override
    public int hashCode() {
        return Objects.hash(classCoverageMaps);
    }
}
