package eu.stamp_project.dspot.common.configuration.options;

import eu.stamp_project.dspot.amplifier.amplifiers.*;

public enum AmplifierEnum {

    // Method Call Amplifiers
    MethodDuplicationAmplifier(new MethodDuplicationAmplifier()),
    MethodRemoveAmplifier(new MethodRemoveAmplifier()),
    MethodAdderOnExistingObjectsAmplifier(new MethodAdderOnExistingObjectsAmplifier()),
    ReturnValueAmplifier(new ReturnValueAmplifier()),

    FastLiteralAmplifier(new FastLiteralAmplifier()),

    // Simple Literal Amplifiers
    StringLiteralAmplifier(new StringLiteralAmplifier()),
    NumberLiteralAmplifier(new NumberLiteralAmplifier()),
    BooleanLiteralAmplifier(new BooleanLiteralAmplifier()),
    CharLiteralAmplifier(new CharLiteralAmplifier()),
    AllLiteralAmplifiers(new AllLiteralAmplifiers()),

    ArrayAmplifier(new ArrayLiteralAmplifier()),
    //    ReplacementAmplifier(new ReplacementAmplifier()),
    NullifierAmplifier(new NullifierAmplifier()),
    None(null);

    private final Amplifier amplifier;

    public Amplifier getAmplifier() {
        return this.amplifier;
    }

    AmplifierEnum(Amplifier amplifier) {
        this.amplifier = amplifier;
    }
}
