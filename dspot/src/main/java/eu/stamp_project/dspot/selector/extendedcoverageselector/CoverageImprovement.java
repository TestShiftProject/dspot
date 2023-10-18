package eu.stamp_project.dspot.selector.extendedcoverageselector;

import java.util.Objects;

public class CoverageImprovement {

    public CoverageImprovement(ProjectCoverageMap instructionDiff) {
        this.instructionImprovement = instructionDiff;
    }

    public CoverageImprovement(CoverageImprovement coverageImprovement) {
        this.instructionImprovement = coverageImprovement.getInstructionImprovement();
    }

    /**
     * For each class name (FQ), for each line number (jacoco line with statements!) the improvement in coverage
     */
    protected final ProjectCoverageMap instructionImprovement;

    public ProjectCoverageMap getInstructionImprovement() {
        return instructionImprovement;
    }

    /**
     * @return the total sum of additional instructions covered.
     */
    public int totalCoverageIncrease() {
        return instructionImprovement.sum();
    }

    @Override
    public String toString() {
        StringBuilder explanation = new StringBuilder("Coverage improved at\n");
        this.instructionImprovement.classCoverageMaps.forEach((className, classCoverageMap) -> {
            explanation.append(className).append(":\n");
            classCoverageMap.methodCoverageMap.forEach((methodName, methodCoverage) -> {
                explanation.append(methodName).append("\n");
                int index = -1;
                for (Integer instructionImprovement : methodCoverage.lineCoverage) {
                    index++;
                    if (instructionImprovement <= 0) {
                        continue;
                    }
                    explanation.append("L. ").append(index + 1)
                            .append(" +").append(instructionImprovement)
                            .append(" instr.").append("\n");
                }
            });
        });
        explanation.replace(explanation.length() - 1, explanation.length(), "");
        return explanation.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        CoverageImprovement that = (CoverageImprovement) o;
        return Objects.equals(instructionImprovement, that.instructionImprovement);
    }

    @Override
    public int hashCode() {
        return Objects.hash(instructionImprovement);
    }
}
