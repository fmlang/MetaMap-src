package gov.nih.nlm.nls.metamap;

/**
 * A representation of MetaMap Phrase structure.
 *
 * <p>
 * Created: Mon May 11 15:17:15 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public interface Phrase extends MetaMapElement {
  String getPhraseText() throws Exception;
  String getMincoManAsString();
  Position getPosition() throws Exception;
}
