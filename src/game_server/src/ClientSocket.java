// Client Side
import java.io.*;
import java.net.*;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpInputStream;
import com.ericsson.otp.erlang.OtpOutputStream;

public class ClientSocket {
    public void run() throws OtpErlangDecodeException {
	try {
	    int serverPort = 3010;
	    InetAddress host = InetAddress.getByName("127.0.0.1"); 
	    System.out.println("Connecting to server on port " + serverPort); 

	    Socket socket = new Socket(host,serverPort); 
	    //Socket socket = new Socket("127.0.0.1", serverPort);
	    System.out.println("Just connected to " + socket.getRemoteSocketAddress()); 
	   /* PrintWriter toServer = 
		new PrintWriter(socket.getOutputStream(),true);
	    // BufferedReader fromServer = 
	    // new BufferedReader(new InputStreamReader
	    // (socket.getInputStream()));
	    */
	    
	    OutputStream out = socket.getOutputStream(); 
	    DataOutputStream dos = new DataOutputStream(out);
	    
	    OtpErlangTuple availableTuple = new OtpErlangTuple(new OtpErlangAtom("available"));
		OtpOutputStream availableStream = new OtpOutputStream(availableTuple);
		byte[] tmp = availableStream.toByteArray();
		byte[] prepend = {(byte)131};
		byte[] data = new byte[prepend.length + tmp.length];
		System.arraycopy(prepend, 0, data, 0, prepend.length);
		System.arraycopy(tmp, 0, data, prepend.length, tmp.length);
	    
	    dos.write(data);
	    
	    //toServer.println("Hello from " + socket.getLocalSocketAddress()); 

	    /* DataInputStream to read from TCP */
	    DataInputStream fromServer = 
		new DataInputStream(socket.getInputStream());
	    //int receivedSize = fromServer.readInt();
	    //System.out.println("Size: " + receivedSize);
	    byte[] message = new byte[248];
	    fromServer.read(message);
	    
		//socket.receive( packet ) ;
		
		OtpErlangObject answer = (new OtpInputStream(message)).read_any();
		System.out.println(answer);
	    /*
	    int pos0 = message[0];
	    int pos1 = message[1];
	    int pos2 = message[2];
	    int pos3 = message[3];

	    // readLine waits for <<10>>, or "\n"
	    // String line = fromServer.readLine();
	    System.out.println("Client received: " + "<<" + pos0 + "," + 
			       pos1 + "," + pos2 + "," +  pos3 + 
			       ">> from Server");
	    
	    System.out.println("Press ENTER to close the connection.");
	    System.in.read();
	    */
	    //toServer.close();
	    fromServer.close();
	    socket.close();
	}
	catch(UnknownHostException ex) {
	    ex.printStackTrace();
	}
	catch(IOException e){
	    e.printStackTrace();
	}
    }
	
    public static void main(String[] args) {
	ClientSocket client = new ClientSocket();
	try {
		client.run();
	} catch (OtpErlangDecodeException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
    }
}
