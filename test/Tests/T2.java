class Pair {
    int x;
    int y;
    public Pair(int x1, int y1) {
        x = x1; y = y1;
    }
}

public class T2 {
    public int fst(Pair p) {
        return p.x;
    }

    public int pr() {
        Pair p = new Pair(3, 4);
        return fst(p);
    }
}
