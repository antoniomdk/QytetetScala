package view;

/**
 * @author Antonio Molner
 */
public class SquareView extends javax.swing.JPanel {
  private javax.swing.JLabel label;
  private javax.swing.JScrollPane scrollPane;
  private javax.swing.JTextArea textArea;

  public SquareView() {
    initComponents();
  }

  @SuppressWarnings("unchecked")
  private void initComponents() {
    scrollPane = new javax.swing.JScrollPane();
    textArea = new javax.swing.JTextArea();
    label = new javax.swing.JLabel();

    setBackground(new java.awt.Color(255, 102, 102));
    setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 2));

    textArea.setEditable(false);
    textArea.setColumns(20);
    textArea.setRows(5);
    scrollPane.setViewportView(textArea);

    label.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    label.setText("SQUARE");

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
    this.setLayout(layout);
    layout.setHorizontalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(
                layout.createSequentialGroup()
                    .addGap(10, 10, 10)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                  .addComponent(label, javax.swing.GroupLayout.DEFAULT_SIZE, 476,
                                      Short.MAX_VALUE)
                                  .addComponent(scrollPane))
                    .addGap(10, 10, 10)));
    layout.setVerticalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING,
                layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(label, javax.swing.GroupLayout.PREFERRED_SIZE, 24,
                        javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(5, 5, 5)
                    .addComponent(scrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 122,
                        javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(10, 10, 10)));
  }

  public void update(String content) {
    textArea.setText(content);
    repaint();
    revalidate();
  }
}
