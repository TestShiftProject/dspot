package eu.stamp_project.dspot.selector.branchcoverageselector;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import eu.stamp_project.dspot.selector.branchcoverageselector.BranchCoverage;
import eu.stamp_project.dspot.selector.branchcoverageselector.MethodCoverage;
import eu.stamp_project.dspot.selector.branchcoverageselector.Region;
import eu.stamp_project.dspot.selector.branchcoverageselector.ClassCoverage;

public class TestMethodCoverage {
    public final String testMethodName;

    public final Map<String, ClassCoverage> classCoverageList;

    public TestMethodCoverage(String testMethodName) {
        this.testMethodName = testMethodName;
        this.classCoverageList = new ConcurrentHashMap<>();
    }

    public synchronized void addBranchCoverage(String className, String methodName, Region region, int trueHitCount, int falseHitCount) {
        if (!this.classCoverageList.containsKey(className)) {
            this.classCoverageList.put(className, new ClassCoverage(className));
        }
        this.classCoverageList.get(className).addBranchCoverage(methodName, region, trueHitCount, falseHitCount);
    }

    public synchronized void addLineCoverage(String className, String methodName, int line, int hitCount) {
        if (!this.classCoverageList.containsKey(className)) {
            this.classCoverageList.put(className, new ClassCoverage(className));
        }
        this.classCoverageList.get(className).addLineCoverage(methodName, line, hitCount);
    }

    public Set<String> getClasses() {
        return this.classCoverageList.keySet();
    }

    public String getTestMethodName() {
        return testMethodName;
    }

    public Map<String, ClassCoverage> getClassCoverageList() {
        return classCoverageList;
    }

    public Map<String, MethodCoverage> getCoverageForClass(String className) {
        if(classCoverageList.containsKey(className)) {
            return classCoverageList.get(className).getMethodCoverageList();
        }
        return null;
    }

    public List<BranchCoverage> getBranchCoverageForClassAndMethod(String className, String methodName){
        Map<String, MethodCoverage> classCoverage = getCoverageForClass(className);
        if(classCoverage!=null){
            List<BranchCoverage> result = new ArrayList<>();
            for(Map.Entry<String, MethodCoverage> entry: classCoverage.entrySet())
                if(entry.getKey().contains(methodName)){
                    result.addAll(entry.getValue().getBranchCoverageList());
                }
            return result;
        }
        return null;
    }

    public List<LineCoverage> getLineCoverageForClassAndMethod(String className, String methodName){
        Map<String, MethodCoverage> classCoverage = getCoverageForClass(className);
        if(classCoverage!=null){
            List<LineCoverage> result = new ArrayList<>();
            for(Map.Entry<String, MethodCoverage> entry: classCoverage.entrySet())
                if(entry.getKey().contains(methodName)){
                    result.addAll(entry.getValue().getLineCoverageList());
                }
            return result;
        }
        return null;
    }

    @Override
    public String toString() {
        return "TestMethodBranchCoverage{" +
                "testMethodName='" + testMethodName + '\'' +
                ", classBranchCoverageList=" + classCoverageList +
                '}';
    }
}
