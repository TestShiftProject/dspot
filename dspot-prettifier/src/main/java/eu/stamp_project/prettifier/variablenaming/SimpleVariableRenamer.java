package eu.stamp_project.prettifier.variablenaming;

import eu.stamp_project.prettifier.Main;
import eu.stamp_project.prettifier.Prettifier;
import eu.stamp_project.prettifier.Util;
import spoon.reflect.code.CtLocalVariable;
import spoon.reflect.code.CtVariableRead;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.visitor.filter.TypeFilter;

import java.util.List;
import java.util.stream.Collectors;

public class SimpleVariableRenamer implements Prettifier {

    @Override
    public List<CtMethod<?>> prettify(List<CtMethod<?>> amplifiedTestsToBePrettified) {
        return amplifiedTestsToBePrettified.stream().map(this::prettifyTest).collect(Collectors.toList());
    }

    private CtMethod<?> prettifyTest(CtMethod<?> test) {
        CtMethod<?> prettifiedTest = test.clone();
        // find all new variables named by DSpot
        final List<CtLocalVariable<?>> localVariables = prettifiedTest.getElements(new TypeFilter<>(CtLocalVariable.class));
        int counter = 1;
        for (CtLocalVariable<?> localVariable : localVariables) {
            if (Util.variableNamedByDSpot(localVariable.getSimpleName())) {
                String newName = localVariable.getType().getSimpleName() + counter;
                counter++;

                Main.report.renamingReport.addVariableRenaming(test, localVariable.getSimpleName(), newName);

                // rename usages of variable
                List<CtVariableRead<?>> variableReads =
                        prettifiedTest.getElements(new Util.LOCAL_VARIABLE_READ_FILTER(localVariable));
                variableReads.forEach(ctVariableRead -> ctVariableRead.getVariable().setSimpleName(newName));

                // rename at assignment
                localVariable.setSimpleName(newName);
            }
        }

        return prettifiedTest;
    }

}
