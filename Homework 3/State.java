interface State {
    int size();
    byte[] current();	//returns the array
    boolean swap(int i, int j); //does the swap operation
    							//on indices i and j
}
