public class T3 {
    int x; 
    public void f(int y) {
        x = y;
    }

    public int g() {
        return x;
    }

    public T3 h() {
        return null;
    }

    public T3 l() {
        return h();
    }
}