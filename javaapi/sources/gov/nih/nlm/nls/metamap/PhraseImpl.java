package gov.nih.nlm.nls.metamap;

import se.sics.prologbeans.*;

/**
 * Implementation class of Phrase interface.
 * <p>
 * Created: Fri May 22 09:12:28 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class PhraseImpl implements Phrase {
  Term phraseTerm;
  
  /**
   * Creates a new <code>PhraseImpl</code> instance.
   *
   */
  public PhraseImpl(Term aPhraseTerm) {
    this.phraseTerm = aPhraseTerm;
  }
  public String getPhraseText() throws Exception {
    return TermUtils.getAtomArgument(this.phraseTerm, 1);
  }
  public String getMincoManAsString() {
    return TermUtils.getArgumentAsString(this.phraseTerm, 2);
  }
  public Position getPosition() throws Exception {
    return TermUtils.getPositionArgument(this.phraseTerm, 3);
  }
}
