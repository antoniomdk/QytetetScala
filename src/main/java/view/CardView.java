package view;

/**
 * @author AntonioMolner
 */
public class CardView extends javax.swing.JPanel {
  private javax.swing.JLabel label;
  private javax.swing.JScrollPane scrollPane;
  private javax.swing.JTextArea textArea;

  public CardView() {
    initComponents();
  }

  @SuppressWarnings("unchecked")
  private void initComponents() {
    scrollPane = new javax.swing.JScrollPane();
    textArea = new javax.swing.JTextArea();
    label = new javax.swing.JLabel();

    setBackground(new java.awt.Color(102, 204, 0));
    setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 2));

    textArea.setEditable(false);
    textArea.setColumns(20);
    textArea.setRows(5);
    scrollPane.setViewportView(textArea);

    label.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    label.setText("CARD");

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
    this.setLayout(layout);
    layout.setHorizontalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING,
                layout.createSequentialGroup()
                    .addGroup(
                        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(layout.createSequentialGroup().addContainerGap().addComponent(
                                    label, javax.swing.GroupLayout.DEFAULT_SIZE,
                                javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            .addGroup(layout.createSequentialGroup()
                                          .addGap(10, 10, 10)
                                          .addComponent(scrollPane,
                                              javax.swing.GroupLayout.DEFAULT_SIZE, 526,
                                              Short.MAX_VALUE)))
                    .addGap(10, 10, 10)));
    layout.setVerticalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                          .addComponent(label, javax.swing.GroupLayout.PREFERRED_SIZE, 30,
                              javax.swing.GroupLayout.PREFERRED_SIZE)
                          .addGap(0, 0, 0)
                          .addComponent(scrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 96,
                              javax.swing.GroupLayout.PREFERRED_SIZE)
                          .addContainerGap(10, Short.MAX_VALUE)));
  }

  public void update(String content) {
    textArea.setText(content);
    repaint();
    revalidate();
  }
}
