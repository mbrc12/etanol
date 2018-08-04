import java.util.*;

public class T6 {
    T6 q;
    public int f(int x) {
        int s = 0;
        for (int i = 0; i < x; i++) {
            s += x;
        }
        return s;
    }

    public double g(double x) {
        return Math.sin(x);
    }

    public double g2(double x) {
        double e = 0, fact = 1, xp = 1;
        for (int i = 0; i < 10; i++) {
            e += xp/fact; fact *= i; xp *= x;
        }
        return e;
    }
}