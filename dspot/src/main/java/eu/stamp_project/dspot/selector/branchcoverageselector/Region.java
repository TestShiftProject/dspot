package eu.stamp_project.dspot.selector.branchcoverageselector;

public class Region {
    private final int startLine;

    private final int startColumn;

    private final int endLine;

    private final int endColumn;

    public Region(int startLine, int startColumn, int endLine, int endColumn){
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.endLine = endLine;
        this.endColumn = endColumn;
    }

    public int getStartLine() {
        return startLine;
    }

    @Override
    public String toString() {
        return "Region{" + startLine + ","+startColumn+","+endLine+","+endColumn+ "}";
    }

}
