package eu.stamp_project.dspot.selector.branchcoverageselector;

import eu.stamp_project.dspot.selector.branchcoverageselector.MethodCoverage;
import eu.stamp_project.dspot.selector.branchcoverageselector.Region;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ClassCoverage {
    public final String className;

    public final Map<String, MethodCoverage> methodCoverageList;

    public ClassCoverage(String className) {
        this.className = className;
        this.methodCoverageList = new ConcurrentHashMap<>();
    }

    public void addBranchCoverage(String methodName, Region region, int trueHitCount, int falseHitcount) {
        if(!this.methodCoverageList.containsKey(methodName)) {
            methodCoverageList.put(methodName, new MethodCoverage(methodName));
        }
        this.methodCoverageList.get(methodName).addBranchCoverage(region, trueHitCount, falseHitcount);
    }

    public void addLineCoverage(String methodName, int line, int hitCount){
        if(!this.methodCoverageList.containsKey(methodName)) {
            methodCoverageList.put(methodName, new MethodCoverage(methodName));
        }
        this.methodCoverageList.get(methodName).addLineCoverage(line, hitCount);
    }

    public Map<String, MethodCoverage> getMethodCoverageList() {
        return methodCoverageList;
    }

    public MethodCoverage getCoverageForMethod(String methodName){
        if(methodCoverageList.containsKey(methodName))
        return methodCoverageList.get(methodName);
        return null;
    }

    @Override
    public String toString() {
        return "ClassBranchCoverage{" +
                "className='" + className + '\'' +
                ", classBranchCoverages=" + methodCoverageList +
                '}';
    }
}
