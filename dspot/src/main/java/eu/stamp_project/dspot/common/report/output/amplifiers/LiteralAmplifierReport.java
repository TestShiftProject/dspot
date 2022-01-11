package eu.stamp_project.dspot.common.report.output.amplifiers;

import eu.stamp_project.dspot.common.miscellaneous.TypeUtils;
import eu.stamp_project.dspot.common.report.output.AmplifierReport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.reflect.code.CtExpression;
import spoon.reflect.code.CtInvocation;
import spoon.reflect.code.CtLocalVariable;
import spoon.reflect.declaration.CtClass;
import spoon.reflect.declaration.CtElement;
import spoon.reflect.declaration.CtExecutable;
import spoon.reflect.declaration.CtParameter;

import java.util.Optional;

/**
 * Reports any changes where a literal was changed to another value.
 * E.g. by the {@link eu.stamp_project.dspot.amplifier.amplifiers.FastLiteralAmplifier},
 * or the {@link eu.stamp_project.dspot.amplifier.amplifiers.NullifierAmplifier}
 */
public class LiteralAmplifierReport extends AmplifierReport {

    private static final Logger LOGGER = LoggerFactory.getLogger(LiteralAmplifierReport.class);

    private final String variableName;
    private String originalValue;
    private String newValue;
    private String type;
    /**
     * Whether the literal is assigned to a local variable.
     */
    private final boolean isLocalVariable;
    /**
     * Only defined if the literal was not assigned to a local variable (and therefore used as a parameter).
     */
    private final String methodName;

    public LiteralAmplifierReport(CtExpression<?> literal, Object newValue) {

        this.originalValue = literal.toString();
        this.newValue = newValue.toString();
        this.type = literal.getType().toString();

        // Determine what literal occurrence we have:
        CtElement parent = TypeUtils.getFirstStatementParent(literal);
        // - assignment to local variable
        if (parent instanceof CtLocalVariable<?>) {
            this.variableName = ((CtLocalVariable<?>) parent).getSimpleName();
            this.isLocalVariable = true;
            this.methodName = "";
        }
        // - method/constructor parameter
        else if (parent instanceof CtInvocation<?>) {
            int literalIndex = ((CtInvocation<?>) parent).getArguments().indexOf(literal);
            if (literalIndex == -1) { // not a parameter -> parts of a parameter were changed
                // find the argument of the invocation which is a parent of the literal and use that index instead

                Optional<CtExpression<?>> argumentWithLiteral =
                        ((CtInvocation<?>) parent).getArguments().stream().filter(literal::hasParent).findFirst();
                if (argumentWithLiteral.isPresent()) {
                    CtExpression<?> argument = argumentWithLiteral.get();
                    literalIndex = ((CtInvocation<?>) parent).getArguments().indexOf(argument);
                    this.originalValue = argument.toString();
                    this.newValue = argument.toString().replace(literal.toString(), newValue.toString());
                    this.type = argument.getType().toString();
                }
            }

            if (literalIndex != -1) { // literal index is now set to a useful value
                CtExecutable<?> methodCalled = ((CtInvocation<?>) parent).getExecutable().getDeclaration();
                CtParameter<?> parameter = methodCalled.getParameters().get(literalIndex);
                this.variableName = parameter.getSimpleName();
                this.methodName =
                        methodCalled.getParent(CtClass.class).getSimpleName() + "." + methodCalled.getSimpleName();
                this.isLocalVariable = false;
            } else {
                // this means the literal is involved in the top statement invocation but not part of the
                // arguments. It can only be in the object reference then? -> we don't expect this to happen
                LOGGER.warn("Failed to report modification for literal {} that is part of the invocation {} but " +
                        "not in its argument", literal.toString(), parent.toString());
                this.variableName = "unknown";
                this.isLocalVariable = false;
                this.methodName = "";
            }

        } else {
            LOGGER.warn("Tried to report modification for literal {} (changed to {}) that is not in variable " +
                    "declaration or method call!", literal.toString(), newValue.toString());
            this.variableName = "unknown";
            this.isLocalVariable = false;
            this.methodName = "";
        }
    }

    @Override
    public boolean isAssertionReport() {
        return false;
    }

    public String getVariableName() {
        return variableName;
    }

    public String getOriginalValue() {
        return originalValue;
    }

    public String getNewValue() {
        return newValue;
    }

    public boolean isLocalVariable() {
        return isLocalVariable;
    }

    public String getMethodName() {
        return methodName;
    }

    public String getType() {
        return type;
    }
}
