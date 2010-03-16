package gov.nih.nlm.nls.metamap;

import java.util.List;

/**
 * Representation of MetaMap Evaluation (Ev) element.
 *<p>
 *
 * Created: Wed Apr 29 16:01:18 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public interface Ev extends MetaMapElement {
  int getScore() throws Exception;
  String getConceptId() throws Exception;
  String getConceptName() throws Exception;
  String getPreferredName() throws Exception;
  List<String> getMatchedWords() throws Exception;
  List<String> getSemanticTypes() throws Exception;
  boolean isHead() throws Exception;
  boolean isOvermatch() throws Exception;
  List<String> getSources() throws Exception;
  List<Position> getPositionalInfo() throws Exception;
}
