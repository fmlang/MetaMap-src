package gov.nih.nlm.nls.metamap;

import java.util.List;

/**
 * Representation of MetaMap Utterance elements.
 *
 * <p>
 * Created: Mon May 11 15:17:56 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public interface Utterance extends MetaMapElement {
  /** Get id of utterance.
   * @return id of utterance */
  String getId() throws Exception;
  /** Get content of utterance. @return utterance string */
  String getString() throws Exception;
  /** Get character position of utterance input text. 
   * @return position of utterance in input text */
  Position getPosition() throws Exception;
  /**
   * Return a list of <code>PCM</code> (Phrase/Candidate/Mapping)
   * objects associated with the utterance.
   * @return a list of <code>PCM</code> (Phrase/Candidate/Mapping) objects.
   */
  List<PCM> getPCMList() throws Exception;
  /** Get term position of the utterance in the MetaMap machine output.
   * @return position of utterance term in machine output*/
}
