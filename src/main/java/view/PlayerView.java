package view;

/**
 * @author Antonio Molner
 */
public class PlayerView extends javax.swing.JPanel {
  private javax.swing.JLabel label;
  private javax.swing.JScrollPane scrollPane;
  private javax.swing.JTextArea textArea;

  public PlayerView() {
    initComponents();
  }

  @SuppressWarnings("unchecked")
  private void initComponents() {
    scrollPane = new javax.swing.JScrollPane();
    textArea = new javax.swing.JTextArea();
    label = new javax.swing.JLabel();

    setBackground(new java.awt.Color(255, 255, 102));
    setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 2));

    textArea.setEditable(false);
    textArea.setColumns(20);
    textArea.setRows(5);
    scrollPane.setViewportView(textArea);

    label.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    label.setText("PLAYER");

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
    this.setLayout(layout);
    layout.setHorizontalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                          .addGap(10, 10, 10)
                          .addComponent(scrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 526,
                              Short.MAX_VALUE)
                          .addGap(10, 10, 10))
            .addComponent(label, javax.swing.GroupLayout.Alignment.TRAILING,
                javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE,
                Short.MAX_VALUE));
    layout.setVerticalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING,
                layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(label, javax.swing.GroupLayout.PREFERRED_SIZE, 21,
                        javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED,
                        javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(scrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 150,
                        javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(10, 10, 10)));
  }

  public void update(String content) {
    textArea.setText(content);
    repaint();
    revalidate();
  }
}
