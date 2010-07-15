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
   Phrase getPhrase() throws Exception ;
   List<Ev> getCandidates() throws Exception ;
   List<Ev> getCandidateList() throws Exception ;
   List<Map> getMappings() throws Exception ;
   List<Mapping> getMappingList() throws Exception ;
}
