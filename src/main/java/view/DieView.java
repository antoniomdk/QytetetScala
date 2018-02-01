package view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Random;
import javax.swing.Timer;

/**
 * @author Francisco Velasco
 * modified by Antonio Molner
 */
public class DieView extends javax.swing.JDialog {

  private static DieView instance = null;
  private Random generator=new Random();
  private Timer timerDice;
  private int value;

  private DieView(java.awt.Frame parent) {
    super(parent, true);

    initComponents();
    timerDice = new Timer (50,diceAction);
    this.addWindowListener(new java.awt.event.WindowAdapter() {
      @Override
      public void windowClosing(java.awt.event.WindowEvent e) {
        System.exit(0);
      }
    });
  }

  public static void createInstance (java.awt.Frame parent) {
    if (instance == null)
      instance = new DieView(parent);
  }

  public static DieView getInstance() {
    return instance;
  }

  private int privateNextNumber() {
    return (generator.nextInt(6)+1);
  }

  private ActionListener diceAction = new ActionListener() {
    @Override
    public void actionPerformed (ActionEvent ev) {
      value = privateNextNumber();
      jL_dice.setText(Integer.toString(value));
      pack();
    }
  };

  public int nextNumber () {
    return nextNumber ("", "");

  }
  public int nextNumber (String message1, String message2) {
    jB_OK.setVisible(false);
    jL_message1.setText(message1);
    jL_message2.setText(message2);
    pack();
    timerDice.start();
    this.setVisible(true);
    return value;
  }

  @SuppressWarnings("unchecked")
  private void initComponents() {
    jL_dice = new javax.swing.JLabel();
    jL_message1 = new javax.swing.JLabel();
    jB_OK = new javax.swing.JButton();
    jLabel1 = new javax.swing.JLabel();
    jL_message2 = new javax.swing.JLabel();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Die");
    setMinimumSize(new java.awt.Dimension(400, 280));
    setPreferredSize(new java.awt.Dimension(400, 280));

    jL_dice.setBackground(new java.awt.Color(255, 255, 255));
    jL_dice.setFont(new java.awt.Font("Trebuchet MS", 2, 48)); // NOI18N
    jL_dice.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    jL_dice.setText("1");
    jL_dice.setOpaque(true);
    jL_dice.addMouseListener(new java.awt.event.MouseAdapter() {
      public void mouseClicked(java.awt.event.MouseEvent evt) {
        jL_diceMouseClicked(evt);
      }
    });

    jL_message1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    jL_message1.setText("jLabel1");

    jB_OK.setText("OK");
    jB_OK.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        jB_OKActionPerformed(evt);
      }
    });

    jLabel1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    jLabel1.setText("pincha sobre el dado para detenerlo");

    jL_message2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    jL_message2.setText("jLabel1");

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                            .addGap(20, 20, 20)
                            .addComponent(jL_message1, javax.swing.GroupLayout.PREFERRED_SIZE, 350, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                            .addGap(20, 20, 20)
                            .addComponent(jL_message2, javax.swing.GroupLayout.PREFERRED_SIZE, 350, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                            .addGap(70, 70, 70)
                            .addComponent(jLabel1))
                    .addGroup(layout.createSequentialGroup()
                            .addGap(160, 160, 160)
                            .addComponent(jL_dice, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                            .addGap(170, 170, 170)
                            .addComponent(jB_OK))
    );
    layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                            .addGap(10, 10, 10)
                            .addComponent(jL_message1, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGap(0, 0, 0)
                            .addComponent(jL_message2, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGap(10, 10, 10)
                            .addComponent(jLabel1)
                            .addGap(13, 13, 13)
                            .addComponent(jL_dice, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGap(10, 10, 10)
                            .addComponent(jB_OK))
    );

    setSize(new java.awt.Dimension(400, 284));
    setLocationRelativeTo(null);
  }

  private void jL_diceMouseClicked(java.awt.event.MouseEvent evt) {
    timerDice.stop();
    jB_OK.setVisible(true);
    pack();
  }

  private void jB_OKActionPerformed(ActionEvent evt) {
    this.dispose();
  }

  private javax.swing.JButton jB_OK;
  private javax.swing.JLabel jL_dice;
  private javax.swing.JLabel jL_message1;
  private javax.swing.JLabel jL_message2;
  private javax.swing.JLabel jLabel1;
}