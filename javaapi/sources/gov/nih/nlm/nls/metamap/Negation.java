package gov.nih.nlm.nls.metamap;

import java.util.List;

/**
 * Represention of MetaMap Negation elements.
 *
 * <p>
 * Created: Mon May 11 15:16:37 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public interface Negation extends MetaMapElement {
  String getType() throws Exception;
  String getTrigger() throws Exception;
  List<Position> getTriggerPositionList() throws Exception;
  List<ConceptPair> getConceptPairList() throws Exception;
  List<Position> getConceptPositionList() throws Exception;
  String getConceptId() throws Exception;
}
