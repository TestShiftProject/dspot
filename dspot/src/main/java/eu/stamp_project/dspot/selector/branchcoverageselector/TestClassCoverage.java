package eu.stamp_project.dspot.selector.branchcoverageselector;

import eu.stamp_project.dspot.selector.branchcoverageselector.TestMethodCoverage;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class TestClassCoverage {
    public final String testClassName;

    public final Map<String, TestMethodCoverage> testMethodsCoverage;

    public TestClassCoverage(String testClassName) {
        this.testClassName = testClassName;
        this.testMethodsCoverage = new ConcurrentHashMap<>();
    }

    public synchronized void addBranchCoverage(String testMethodName, String className, String methodName, Region region, int trueHitCount, int falseHitCount) {
        if (!this.testMethodsCoverage.containsKey(testMethodName)) {
            this.testMethodsCoverage.put(testMethodName, new TestMethodCoverage(testMethodName));
        }
        this.testMethodsCoverage.get(testMethodName).addBranchCoverage(className, methodName, region, trueHitCount, falseHitCount);
    }

    public synchronized void addLineCoverage(String testMethodName, String className, String methodName, int line, int hitCount) {
        if (!this.testMethodsCoverage.containsKey(testMethodName)) {
            this.testMethodsCoverage.put(testMethodName, new TestMethodCoverage(testMethodName));
        }
        this.testMethodsCoverage.get(testMethodName).addLineCoverage(className, methodName, line, hitCount);
    }

    public Map<String, TestMethodCoverage> getTestMethodsCoverage() {
        return testMethodsCoverage;
    }

//    public Set<String> getTestMethods() {
//        return this.testMethodsCoverage.keySet();
//    }

//    public Set<String> getClassesForTestMethodName(String testMethodName) {
//        return this.testMethodsCoverage.get(testMethodName).getClasses();
//    }

    public Map<String, MethodCoverage> getCoverageForTestMethodAndClassNames(String testMethodName, String className) {
        return this.testMethodsCoverage.get(testMethodName).getCoverageForClass(className);
    }

    public TestMethodCoverage getTestMethodCoverage(String testMethodName) {
        if(testMethodsCoverage.containsKey(testMethodName)) {
            return this.testMethodsCoverage.get(testMethodName);
        }
        return null;
    }

    @Override
    public String toString() {
        return "TestClassBranchCoverage{" +
                "testClassName='" + testClassName + '\'' +
                ", testMethodsBranchCoverage=" + testMethodsCoverage +
                '}';
    }
}
