package gov.nih.nlm.nls.metamap;

import java.util.List;

/**
 * <p>
 * Interface for container for a Phrase, Candidates, and Mappings set.
 * </p>
 *
 * Created: Thu May 21 17:44:31 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public interface PCM extends MetaMapElement {
  /** Return the {@link Phrase} associated with set of Mappings and Candidates.
   * @return the {@link Phrase} instance.
   */
   Phrase getPhrase() throws Exception ;
   List<Ev> getCandidates() throws Exception ;
  /** Return the Candidate ({@link Ev}) list associated with the {@link Phrase}.
   * @return a List of {@link Ev} candidate instances.
   */
   List<Ev> getCandidateList() throws Exception ;
   List<gov.nih.nlm.nls.metamap.Map> getMappings() throws Exception ;
  /** Return the {@link Mapping} list associated with the {@link Phrase}.
   * @return a List of {@link Mapping} instances.
   */
   List<Mapping> getMappingList() throws Exception ;
}
