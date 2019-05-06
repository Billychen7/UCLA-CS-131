import java.util.concurrent.ThreadLocalRandom;

class SwapTest implements Runnable { //Runnable is an interface that
    private int nTransitions;		 //defines that a Thread object
    private State state;			 //can run this code

    SwapTest(int n, State s) {
		nTransitions = n;
		state = s;
    }
    
    //runs state.swap(a,b) with random values a and b
    //as many times as specified
    public void run() {
		int n = state.size();
		if (n != 0)
		    for (int i = 0; i < nTransitions; ) {
				int a = ThreadLocalRandom.current().nextInt(0, n);
				int b = ThreadLocalRandom.current().nextInt(0, n - 1);
				if (a == b)
				    b = n - 1;
				if (state.swap(a, b)) //calls swap with 2 random
				    i++;			  //indices, a and b
		    }
    }
}
