/******************************************************************************/
/*                                                                            */
/*                                                  FILE: DatagramClient.java */
/*                                                                            */
/*  Demonstrates a simple datagram client                                     */
/*  =====================================                                     */
/*                                                                            */
/*  V1.00   16-DEC-1998 Te                                                    */
/*  V1.10   12-OCT-2009 Te Cleaned up and extended                            */
/*                                                                            */
/******************************************************************************/

import java.net.* ;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.*; 

/**
 *  A simple datagram client
 *  Shows how to send and receive UDP packets in Java
 *
 *  @author  P. Tellenbach,  http://www.heimetli.ch
 *  @version V1.00
 */
public class DatagramClient
{
    private final static int PACKETSIZE = 100 ;

    public static void main( String args[] )
    {
	// Check the arguments
	/*if( args.length != 2 )
	    {
		System.out.println( "usage: java DatagramClient host port" ) ;
		return ;
	    }
*/
	DatagramSocket socket = null ;

	try
	    {
		// Convert the arguments first, to ensure that they are valid
		//InetAddress host = InetAddress.getByName( args[0] ) ;
		//int port         = Integer.parseInt( args[1] ) ;
		
		InetAddress host = InetAddress.getByName( "localhost" ) ;
		int port         = 4000; //Integer.parseInt( args[1] ) ;
		
		// Construct the socket
		socket = new DatagramSocket() ;
		
		// Construct the datagram packet
		OtpErlangTuple availableTuple = new OtpErlangTuple(new OtpErlangAtom("available"));
		OtpOutputStream availableStream = new OtpOutputStream(availableTuple);
		byte[] tmp = availableStream.toByteArray();
		byte[] prepend = {(byte)131};
		byte[] data = new byte[prepend.length + tmp.length];
		System.arraycopy(prepend, 0, data, 0, prepend.length);
		System.arraycopy(tmp, 0, data, prepend.length, tmp.length);
		
		//System.out.println(data);
		
		
		DatagramPacket packet = new DatagramPacket( data, data.length, host, port ) ;

		// Send it
		socket.send( packet ) ;

		// Set a receive timeout, 2000 milliseconds
		socket.setSoTimeout( 5000 ) ;

		// Prepare the packet for receive
		packet.setData( new byte[PACKETSIZE] ) ;

		// Wait for a response from the server
		socket.receive( packet ) ;
	
		OtpErlangObject answer = (new OtpInputStream(packet.getData())).read_any();
		System.out.println(answer);

		
		
		
		

		// Print the response
		//System.out.println( new String(packet.getData()) ) ;
         
	    }
	catch( Exception e )
	    {
		System.out.println( e ) ;
	    }
	finally
	    {
		if( socket != null )
		    socket.close() ;
	    }
    }
}
