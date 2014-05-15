import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class PanelRetriever{

    Menu mainMenu;
    BrowserMenu bm;
    InputHandler input;

    public PanelRetriever(final JFrame frame){
        //Build the first panel
    	mainMenu = new Menu();
    	mainMenu.setSbButton(new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent arg0) {
    		frame.setContentPane(getPanel2());
    		frame.validate();
    		}});
    	
        mainMenu.add(Box.createVerticalGlue());

        //Build second panel
        bm = new BrowserMenu(frame);
        bm.setOpaque(true);
        input = new InputHandler(frame);
    }

    public Container getPanel1(){
        return mainMenu;
    }

    public Container getPanel2(){
        return bm;
    }

    public static void main(String args[])
    {
        EventQueue.invokeLater(new Runnable()
        {
            public void run()
            {
                JFrame frame = new JFrame();
                PanelRetriever pr = new PanelRetriever(frame);
                frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
                frame.setContentPane(pr.getPanel1());
                frame.setPreferredSize(new Dimension(500, 400));
                frame.pack();
                frame.setVisible(true);
            }
        });
    }
}