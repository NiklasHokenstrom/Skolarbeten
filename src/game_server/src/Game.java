
//import InputHandler;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.ArrayList;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import com.ericsson.otp.erlang.OtpErlangDecodeException;

/**
 * Main class for the game
 */
public class Game extends JFrame
{       
	static String  player123 = "player1";
        /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
		boolean isRunning = true;
        int fps = 30;
        int windowWidth = 500;
        int windowHeight = 500;
       
        BufferedImage backBuffer;
        Insets insets;
        InputHandler input;
       
        int x = 0;
        //int[] ip;
        //Jinterface_bank_client client;
        //Player player;
        //static int[] ip = {127,0,0,1};
       
        public Game() {
        	//this.ip = ip;
        	
            setTitle("Game Tutorial");
            setSize(windowWidth, windowHeight);
            setResizable(false);
            setDefaultCloseOperation(EXIT_ON_CLOSE);

            insets = getInsets();
            setSize(insets.left + windowWidth + insets.right,
            		insets.top + windowHeight + insets.bottom);

            backBuffer = new BufferedImage(windowWidth, windowHeight, BufferedImage.TYPE_INT_RGB);
            input = new InputHandler(this);
            setFocusable(true);
           // setRequestFocusEnabled(true);
            setVisible(true);
           // this.toFront();  
        }

        
        public static void main(String[] args)
        {
        		int[] local = {127,0,0,1};
                //Game game = new Game(local);
                final Jinterface_bank_client client = new Jinterface_bank_client("127.0.0.1", 3010);
                client.add("newServ", local);
                client.available();
                final Player clientPlayer = new Player(10,10, "player1");
                clientPlayer.addPlayerToServer(local, client);
                Player player2 = new Player(20,20, "player2");
                player2.addPlayerToServer(local, client);
               
                final Game game = new Game();/*
               SwingWorker worker = new SwingWorker<Void, Void>() {
                	@Override
                	public Void doInBackground() {
                		game.run(client, clientPlayer);
                		return null;
                	}
                };*/
                game.run(client, clientPlayer);
        		//System.exit(0);
        		/*
                SwingUtilities.invokeLater(new Runnable() {
                	public void run () {

                		Game game = new Game(local);
                		ArrayList<Player> playerList = new ArrayList<Player>();
                		//playerList.add(player1);
                		//playerList.add(player2);

                	
                		// SwingUtilities.invokeLater(game.runGame);
                		game.run(client, clientPlayer);
                		System.exit(0);
                	}
                });
            */
        }
       
        /**
         * This method starts the game and runs it in a loop
         */
        //Runnable runGame = new Runnable() { 
        public void run(final Jinterface_bank_client client, final Player clientPlayer)
        {
        	System.out.println("In run method...");
        	//initialize();
        	//SwingWorker worker = new SwingWorker<Void, Void>() {
        		//@Override
        		//public Void doInBackground() {
        			while(isRunning)
        			{
        				
        				System.out.println("Focusable " + this.isFocusable());
        				System.out.println("Enabled: " + isEnabled());
        				System.out.println("Displayable: " + isDisplayable());
        				System.out.println("Visible: " + isVisible());
        				requestFocus();
        				System.out.println("Request focus in window: " + requestFocusInWindow());
        				System.out.println("Focused : " +this.isFocused());
        				System.out.println("Focus Owner: " + getFocusOwner());

        				long time = System.currentTimeMillis();
        				final ArrayList<Player> playerList = client.getAllPos();
        				update(client, clientPlayer);
        				System.out.println("Event is Dispatch thread: " +SwingUtilities.isEventDispatchThread());
        				System.out.println();
        				draw(playerList);
        				//  draw(playerList);

        				//  delay for each frame  -   time it took for one frame
        				time = (1000 / fps) - (System.currentTimeMillis() - time);

        				if (time > 0)
        				{
        					try
        					{
        						Thread.sleep(time);
        					}
        					catch(Exception e){}
        				}
        			}
        			//return null;
        		//}
        	//};

        	setVisible(false);
        }
       
        /**
         * This method will set up everything need for the game to run
         */
       /* void initialize()
        {
                setTitle("Game Tutorial");
                setSize(windowWidth, windowHeight);
                setResizable(false);
                setDefaultCloseOperation(EXIT_ON_CLOSE);
                setVisible(true);
               
                insets = getInsets();
                setSize(insets.left + windowWidth + insets.right,
                                insets.top + windowHeight + insets.bottom);
               
                backBuffer = new BufferedImage(windowWidth, windowHeight, BufferedImage.TYPE_INT_RGB);
                input = new InputHandler(this);
        }*/
       
        /**
         * This method will check for input, move things
         * around and check for win conditions, etc
         * @throws OtpErlangDecodeException 
         */
        void update(Jinterface_bank_client client, Player playerObj)
        {
        		System.out.println("__________Updating_________");
                if (input.isKeyDown(KeyEvent.VK_RIGHT))
                {
                        //x += 5;
                	client.move(playerObj.getPlayerName(), "right", 5);
                }
                if (input.isKeyDown(KeyEvent.VK_LEFT))
                {
                	client.move(playerObj.getPlayerName(), "left", 5);
                //	x -= 5;
                }
                if (input.isKeyDown(KeyEvent.VK_DOWN))
                {
                        //x += 5;
                	client.move(playerObj.getPlayerName(), "down", 5);
                }
                if (input.isKeyDown(KeyEvent.VK_UP))
                {
                	client.move(playerObj.getPlayerName(), "up", 5);
                //	x -= 5;
                }
               client.updatePos(playerObj.getPlayerName(), playerObj);
        }
       
        /**
         * This method will draw everything
         */
        // void draw(ArrayList<Player> playerList)
         void draw(ArrayList<Player> playerList)
        {       
                Graphics g = getGraphics();
               
                Graphics bbg = backBuffer.getGraphics();
               
                bbg.setColor(Color.WHITE);
                bbg.fillRect(0, 0, windowWidth, windowHeight);
               
                bbg.setColor(Color.BLACK);
                for(Player player : playerList) {
                	bbg.drawOval(player.getX(), player.getY(), 20, 20);
                }
               
                g.drawImage(backBuffer, insets.left, insets.top, this);
        } 
}