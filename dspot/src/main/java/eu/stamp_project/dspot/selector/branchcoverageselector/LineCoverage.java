package eu.stamp_project.dspot.selector.branchcoverageselector;

public class LineCoverage {
    public final int line;

    private int hitCount;

    public LineCoverage(int line, int hitCount) {
        this.line = line;
        this.hitCount = hitCount;
    }

    public int getLine() {
        return line;
    }

    public int getHitCount() {
        return this.hitCount;
    }

//    public void merge(LineCoverage that) {
//        this.hitCount += that.hitCount;
//    }

    @Override
    public String toString() {
        return "LineCoverage{" +
                "line=" + line +
                ", hitCount=" + hitCount +
                '}';
    }
}
