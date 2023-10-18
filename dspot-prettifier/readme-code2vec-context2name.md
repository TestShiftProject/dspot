# Readme for the Code2Vec and Context2Name Prettifiers

TODO to use options .... follow the instructions in this readme.
Under "Demo" you can also find some examples of the renaming that context2name provides.


## Install
For __code2vec__, you need to run `install_code2vec.sh` to download repo.

## Info
For __context2name__, if you want to train one better model:

1. Prepare corpus. You could run `PGA.java` directly or make some adjustments to customize corpus.
2. Process corpus. You just need to call `fnCorpus()` of `C2N.java`, then `training.csv` and `validation.csv` get generated.
3. Train model. You just need to run `c2n_train.py` with `training.csv`, `validation.csv` and `config.json` at hand. Besides, run `pip3 install bottleneck numpy keras tensorflow` to install lib if necessary.
4. Run one demo. You just need to call `fnDemo()` of `C2N.java`.

## Demo
For __context2name__, here is some methods for demonstration:

before Context2Name
```Java
private void mess(int id) {
    String mess = "mess-print";
    System.out.print(mess);
    String local = "local" + id;
    System.out.print(((this.global) + local));
}
```
after Context2Name
```Java
private void mess(int name) {
    String mess = "mess-print";
    System.out.print(mess);
    String ex = "local" + name;
    System.out.print(((this.global) + ex));
}
```

before Context2Name
```Java
private void test() {
    String mess = "mess-label";
    System.out.print(mess);
    outer : for (int i = 0; i < 10; i++) {
        inner : for (int j = 10; j > 0; j--) {
            if (i != j) {
                System.out.print(((("break as i" + i) + "j") + j));
                break outer;
            } else {
                System.out.print(((("continue as i" + i) + "j") + j));
                continue inner;
            }
        }
    }
}
```
after Context2Name
```Java
private void test() {
    String mess = "mess-label";
    System.out.print(mess);
    tc : for (int c = 0; c < 10; c++) {
        result : for (int gridBagConstraints = 10; gridBagConstraints > 0; gridBagConstraints--) {
            if (c != gridBagConstraints) {
                System.out.print(((("break as i" + c) + "j") + gridBagConstraints));
                break tc;
            } else {
                System.out.print(((("continue as i" + c) + "j") + gridBagConstraints));
                continue result;
            }
        }
    }
}
```

before Context2Name
```Java
private void exception() {
    try {
        throw Exception;
    } catch (Exception ex) {
        ex.printStackTrace();
    }
}
```
after Context2Name
```Java
private void exception() {
    try {
        throw Exception;
    } catch (Exception e) {
        e.printStackTrace();
    }
}
```

before Context2Name
```Java
public static void main(String[] args) {
    System.out.print(Demo.str);
}
```
after Context2Name
```Java
public static void main(String[] i) {
    System.out.print(str);
}
```

If interested, here are complete code files before and after Context2Name:

before Context2Name
```Java
import spoon.Launcher;

public class Demo {
    private static String str = "str";
    private final String mess = "mess-Demo";
    private final String global = "global";

    private void mess(int id) {
        String mess = "mess-print";
        System.out.print(mess);
        String local = "local" + id;
        System.out.print(global + local);
    }

    private void test() {
        String mess = "mess-label";
        System.out.print(mess);
        outer:
        for (int i = 0; i < 10; i++) {
            inner:
            for (int j = 10; j > 0; j--) {
                if (i != j) {
                    System.out.print("break as i" + i + "j" + j);
                    break outer;
                } else {
                    System.out.print("continue as i" + i + "j" + j);
                    continue inner;
                }
            }
        }
    }

    private void exception() {
        try {
            throw Exception;
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public static void main(String[] args) {
        System.out.print(Demo.str);
    }
}
```
after Context2Name
```Java
import spoon.Launcher;

public class Demo {

    private static String str = "str";

    private final String mess = "mess-Demo";

    private final String gridBagConstraints = "global";

    private void mess(int result) {
        String mess = "mess-print";
        System.out.print(mess);
        String context = "local" + result;
        System.out.print(gridBagConstraints + context);
    }

    private void test() {
        String mess = "mess-label";
        System.out.print(mess);
        tc: for (int c = 0; c < 10; c++) {
            ex: for (int name = 10; name > 0; name--) {
                if (c != name) {
                    System.out.print("break as i" + c + "j" + name);
                    break tc;
                } else {
                    System.out.print("continue as i" + c + "j" + name);
                    continue ex;
                }
            }
        }
    }

    private void exception() {
        try {
            throw Exception;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] i) {
        System.out.print(Demo.str);
    }
}
```
