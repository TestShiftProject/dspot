package eu.stamp_project.prettifier.variablenaming;

import eu.stamp_project.prettifier.Prettifier;
import eu.stamp_project.prettifier.variablenaming.context2name.Context2Name;
import spoon.Launcher;
import spoon.reflect.declaration.CtClass;
import spoon.reflect.declaration.CtMethod;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class Context2NameVariableRenamer implements Prettifier {

    @Override
    public List<CtMethod<?>> prettify(List<CtMethod<?>> amplifiedTestsToBePrettified) {
        Context2Name context2name = new Context2Name();
        CtClass<?> tmpClass = Launcher.parseClass("class Tmp {}");
        // remember the order
        List<String> methodNameList = new ArrayList<>();
        for (CtMethod<?> amplifiedTestMethod : amplifiedTestsToBePrettified) {
            methodNameList.add(amplifiedTestMethod.getSimpleName());
        }
        // apply Context2Name
        tmpClass.setMethods(new HashSet<>(amplifiedTestsToBePrettified));
        String strTmpClass = tmpClass.toString();
        String strProcessedClass = context2name.process(strTmpClass);
        CtClass<?> processedClass = Launcher.parseClass(strProcessedClass);
        // restore the order
        List<CtMethod<?>> prettifiedMethodList = new ArrayList<>();
        methodNameList.forEach(methodName -> {
            prettifiedMethodList.addAll(processedClass.getMethodsByName(methodName));
        });
        return prettifiedMethodList;
    }
}
