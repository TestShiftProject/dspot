package eu.stamp_project.dspot.selector.branchcoverageselector;

import java.util.ArrayList;
import java.util.List;

public class MethodCoverage {
    public final String methodName;

    public final List<BranchCoverage> branchCoverageList;
    public final List<LineCoverage> lineCoverageList;

    public MethodCoverage(String methodName){
        this.methodName = methodName;
        this.branchCoverageList = new ArrayList<>();
        this.lineCoverageList = new ArrayList<>();
    }

    public void addBranchCoverage(Region region, int trueHitCount, int falseHitCount) {
        this.branchCoverageList.add(new BranchCoverage(region, trueHitCount, falseHitCount));
    }

    public void addLineCoverage(int line, int hitCount){
        this.lineCoverageList.add(new LineCoverage(line, hitCount));
    }

    public List<BranchCoverage> getBranchCoverageList() {
        return this.branchCoverageList;
    }

    public List<LineCoverage> getLineCoverageList() {
        return lineCoverageList;
    }

    public boolean containsBranch(Region region) {
        return this.branchCoverageList.stream().anyMatch(BranchCoverage -> BranchCoverage.getRegion() == region);
    }

    public String toString() {
        return "MethodBranchCoverage{" +
                "methodName='" + methodName + '\'' +
                ", methodBranchCoverages=" + branchCoverageList + lineCoverageList +
                '}';
    }

}
