package view;

import model.IPlayerStats;

/**
 * @author AntonioMolner
 */
public class QytetetView extends javax.swing.JPanel {
  private CardView cardView;
  private SquareView squareView;
  private PlayerView playerView;

  public QytetetView() {
    initComponents();
  }

  @SuppressWarnings("unchecked")
  private void initComponents() {
    playerView = new PlayerView();
    squareView = new SquareView();
    cardView = new CardView();

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
    this.setLayout(layout);
    layout.setHorizontalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING,
                layout.createSequentialGroup()
                    .addGroup(
                        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(layout.createSequentialGroup().addContainerGap().addComponent(
                                    playerView, javax.swing.GroupLayout.DEFAULT_SIZE, 746,
                                Short.MAX_VALUE))
                            .addGroup(layout.createSequentialGroup().addGap(6, 6, 6).addGroup(
                                layout
                                    .createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(squareView,
                                        javax.swing.GroupLayout.Alignment.TRAILING,
                                        javax.swing.GroupLayout.DEFAULT_SIZE,
                                        javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(cardView,
                                        javax.swing.GroupLayout.DEFAULT_SIZE,
                                        javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
                    .addContainerGap()));
    layout.setVerticalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                          .addContainerGap()
                          .addComponent(playerView, javax.swing.GroupLayout.PREFERRED_SIZE,
                              javax.swing.GroupLayout.DEFAULT_SIZE,
                              javax.swing.GroupLayout.PREFERRED_SIZE)
                          .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                          .addComponent(squareView, javax.swing.GroupLayout.PREFERRED_SIZE,
                              javax.swing.GroupLayout.DEFAULT_SIZE,
                              javax.swing.GroupLayout.PREFERRED_SIZE)
                          .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                          .addComponent(cardView, javax.swing.GroupLayout.PREFERRED_SIZE,
                              javax.swing.GroupLayout.DEFAULT_SIZE,
                              javax.swing.GroupLayout.PREFERRED_SIZE)
                          .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)));
  }

  public void update(model.Game gameState) {
    playerView.update(gameState.player().toString());
    squareView.update(IPlayerStats.currentSquare(gameState, gameState.player()).toString());
    scala.Option<model.Card> card = gameState.currentCard();

    if (card.isDefined())
      cardView.update(card.get().toString());
    else
      cardView.update("");
  }
}
