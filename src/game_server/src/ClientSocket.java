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
	private Socket socket; 
	private OutputStream out;
	private DataOutputStream dos;
	private DataInputStream fromServer;
	
	public ClientSocket (String host, int port) {
		try {
			this.socket = new Socket (InetAddress.getByName(host), port);
			this.out = socket.getOutputStream();
			this.dos = new DataOutputStream(out);
			this.fromServer = new DataInputStream(socket.getInputStream());
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	
    /*public void run() throws OtpErlangDecodeException {
	try {
	    int serverPort = 3010;
	    InetAddress host = InetAddress.getByName("127.0.0.1"); 
	    System.out.println("Connecting to server on port " + serverPort); 

	    Socket socket = new Socket(host,serverPort); 
	    System.out.println("Just connected to " + socket.getRemoteSocketAddress()); 
	    
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
	   */ 
	    //toServer.println("Hello from " + socket.getLocalSocketAddress()); 

	    /* DataInputStream to read from TCP */
	    /*DataInputStream fromServer = 
		new DataInputStream(socket.getInputStream());
	    //int receivedSize = fromServer.readInt();
	    //System.out.println("Size: " + receivedSize);
	    byte[] message = new byte[248];
	    fromServer.read(message);
	    
		//socket.receive( packet ) ;
		
		OtpErlangObject answer = (new OtpInputStream(message)).read_any();
		System.out.println(answer);
	    socket.close();
	}
	catch(UnknownHostException ex) {
	    ex.printStackTrace();
	}
	catch(IOException e){
	    e.printStackTrace();
	}
    }
	*/
	
	public OtpErlangObject sendTCP(OtpErlangObject arg) {
		OtpOutputStream availableStream = new OtpOutputStream(arg);
		byte[] data = Utility.arrayPrepend(availableStream);
		try {
			dos.write(data);
			byte[] message = new byte[248];
			fromServer.read(message);

			OtpErlangObject answer = (new OtpInputStream(message)).read_any();
			return answer;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	/*public OtpErlangObject sendTCP(String arg) {
		//OtpOutputStream availableStream = new OtpOutputStream(arg);
		//byte[] data = Utility.arrayPrepend(availableStream);
		byte[] tmp = arg.getBytes();
		byte[] data = Utility.arrayPrepend(tmp);
		for (int i = 0; i < data.length; i++){
			System.out.print(data[i]);
		}
		System.out.println();
		try {
			dos.write(data);
			byte[] message = new byte[248];
			fromServer.read(message);

			OtpErlangObject answer = (new OtpInputStream(message)).read_any();
			return answer;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}*/
    public static void main(String[] args) {
	ClientSocket client = new ClientSocket("127.0.0.1", 3011);
	OtpErlangAtom add_table = new OtpErlangAtom("add_table");
	OtpErlangObject answer = client.sendTCP(add_table);
	System.out.println(answer);
	
	/*try {
		client.run();
	} catch (OtpErlangDecodeException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}*/
    }
}
