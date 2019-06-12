import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneLayout;

import jess.JessException;
import jess.QueryResult;
import jess.ValueVector;



public class Template extends JFrame implements ActionListener{
	
	private static final long serialVersionUID = 1L;
	
	JLabel [] label = new JLabel [10];
	JTextArea [] txt = new JTextArea [10];
	JButton close = new JButton("Close");
	String []kata = new String[10];
	JScrollPane pane = new JScrollPane();
	JPanel pane2 = new JPanel();
	// inisialisasi vector untuk setiap variabel house
	Vector <String> vecId = new Vector<String>(); 
	Vector <String> vecType = new Vector<String>(); 
	Vector <String> vecRoom = new Vector<String>();
	Vector <String> vecPrice = new Vector<String>(); 
	Vector <String> vecLocation = new Vector<String>(); 
	Vector <String> vecGarage = new Vector<String>(); 
	Vector <String> vecMatch = new Vector<String>();
	// inisialisasi vector untuk  variabel user
	Vector <String> vecName = new Vector<String>(); 
	Vector <String> vecGender = new Vector<String>(); 
	Vector <String> vecPreference = new Vector<String>();
	Vector <String> vecIncome = new Vector<String>(); 
	Vector <String> vecUserLocation = new Vector<String>(); 
	Vector <String> vecUserType = new Vector<String>(); 
	Vector <String> vecCar = new Vector<String>();
	String getQuery = new String();
	String getUserInfo = new String();
	
	public Template(){
		setTitle("Searched House Result");
		setSize(300, 200);
		pane.setLayout(new ScrollPaneLayout());
		pane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		pane2.setLayout(new GridLayout(2, 3));
		
		// check apakah search with garage atau without garage dari getUserPref
		// getQuery -> bila preference with garage, maka data akan diambil dari getHouseWithGarage
		// getQuery -> bila preference without garage, maka data akan diambil dari getHouseNoGarage
		// getUserInfo -> bila preference with garage, maka data akan diambil dari getUserInfoWithGarage
		// getUserInfo -> bila preference without garage, maka data akan diambil dari getUserInfoNoGarage

		try{
			QueryResult preference = Main.rete.runQueryStar("getUserPref", new ValueVector());
			
			if(preference.getString("preference") == "With Garage"){
				getQuery = "getHouseWithGarage";
				getUserInfo = "getUserInfoWithGarage";
			} else if(preference.getString("preference") == "Without Garage"){
				getQuery = "getHouseNoGarage";
				getUserInfo = "getUserInfoNoGarage";
			} else {
				throw new NullPointerException("No Matches Found"); 
			}
			
		} catch (JessException e) {
			e.printStackTrace();
			System.out.println("User Preferences Not Found");
		}

		kata[0] = "House ID";
		kata[1] = "House Type";
		kata[2] = "Room";
		kata[3] = "Price";
		kata[4] = "Location";
		// jika hasil check with garage, maka harus dibuat garage
		// jika hasil check without garage, tidak perlu garage, hanya match rate.
		if(getQuery == "getHouseWithGarage"){
			kata[5] = "Garage";
			kata[6] = "Match Rate";
		} else {
			kata[5] = "Match Rate";
		}
		
		// source diambil dari contoh pada pertemuan 5
		// untuk add data user info
		/*try{
			QueryResult user = Main.rete.runQueryStar(getUserInfo, new ValueVector());
				vecName.add(user.getString("name"));
				vecGender.add(user.getString("gender"));
				vecPreference.add(user.getString("preference"));
				vecIncome.add(user.getString("income"));
				vecUserLocation.add(user.getString("location"));
				vecUserType.add(user.getString("type"));
				if(getUserInfo == "getUserInfoWithGarage"){
					vecCar.add(user.getString("garage"));
				}
		} catch (JessException e) {
			e.printStackTrace();
		}*/
		
		// source diambil dari contoh pada pertemuan 5
		// getQuery bergantung pada user preference
		try{
			QueryResult result = Main.rete.runQueryStar(getQuery, new ValueVector());
			
			while(result.next()){
				vecId.add(result.getString("id"));
				vecType.add(result.getString("type"));
				vecRoom.add(result.getString("room"));
				vecPrice.add(result.getString("price"));
				vecLocation.add(result.getString("location"));
				if(getQuery == "getHouseWithGarage"){
					vecGarage.add(result.getString("garage"));
				}
				vecMatch.add(result.getString("match"));
			}
			
			for (int i = 0; i < vecId.size() ; i++){
				txt[0].append(vecId.get(i) + "\n");
				txt[1].append(vecType.get(i) + "\n");
				txt[2].append(vecRoom.get(i) + "\n");
				txt[3].append(vecPrice.get(i) + "\n");
				txt[4].append(vecLocation.get(i) + "\n");
				if(getQuery == "getHouseWithGarage"){
					txt[5].append(vecGarage.get(i) + "\n");
					txt[6].append(vecMatch.get(i) + "% \n");
				} else {
					txt[5].append(vecMatch.get(i) + "% \n");
				}
			}
		} catch (JessException e) {
			e.printStackTrace();
		}
		
		// bila house with garage berarti membutuhkan 6
		// bila without garage, berarti hanya 5
		if(getQuery == "getHouseWithGarage"){	
			for(int i=0;i<6;i++)
			{
				pane2.add(txt[i]);
			}
		} else {
			for(int i=0;i<5;i++)
			{
				pane2.add(txt[i]);
			}
		}
	
		add(pane);
		pane.getViewport().add(pane2);
		add(close,"South");
		close.addActionListener(this);
		setLocationRelativeTo(null);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		setVisible(true);
		
	}

	public void actionPerformed(ActionEvent arg0) {
		if(arg0.getSource()==close)
			this.dispose();
	}
	
}
