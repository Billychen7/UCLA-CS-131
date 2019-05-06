import java.util.concurrent.atomic.AtomicIntegerArray;

/*
GetNSet uses volatile access to array elements.
It is implemented using the get and set methods of
java.util.concurrent.atomic.AtomicIntegerArray.
*/

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    //used to convert a byte array (the input to the constructor)
    //into an int array, so we can pass that int array to the
    //AtomicIntegerArray constructor
    private int[] byteArrayToIntArray(byte[] b) {
        int arrLength = b.length;
        int[] intArr = new int[arrLength];

        for (int i = 0; i < arrLength; i++) {
            intArr[i] = b[i];
        }

        return intArr;
    }

    GetNSetState(byte[] v) {
        int[] intV = byteArrayToIntArray(v);
        value = new AtomicIntegerArray(intV);
        maxval = 127; 
    }

    GetNSetState(byte[] v, byte m) {
        int[] intV = byteArrayToIntArray(v);
        value = new AtomicIntegerArray(intV); 
        maxval = m; 
    }

    public int size() { return value.length(); }


    //used to convert an AtomicIntArray to a byte array since the
    //current() function returns a byte array, but our value is
    //an AtomicIntArray
    private byte[] AtomicIntArrayToByteArray(AtomicIntegerArray a) {
        int arrLength = a.length();
        byte[] b = new byte[arrLength];

        for (int i = 0; i < arrLength; i++)
        {
            b[i] = (byte) a.get(i); //must cast from int to byte
        }

        return b;
    }

    public byte[] current() { 
        return AtomicIntArrayToByteArray(value); 
    }

    public boolean swap(int i, int j) {
        int iVal = value.get(i);
        int jVal = value.get(j);
    	if (iVal <= 0 || jVal >= maxval) {
    	    return false;
    	}
    	value.set(i, iVal - 1);
    	value.set(j, jVal + 1);
        
    	return true;
    }
}
