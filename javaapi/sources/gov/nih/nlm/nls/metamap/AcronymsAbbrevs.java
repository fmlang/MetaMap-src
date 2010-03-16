package gov.nih.nlm.nls.metamap;

import java.util.List;

/**
 * The interface for MetaMap Acronyms and Abbreviations.
 *
 *<p>
 * Created: Mon May 11 15:21:34 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public interface AcronymsAbbrevs extends MetaMapElement {
  /** get acronym or abbreviation */
  String getAcronym();
  /** get the expansion of the acronym or abbreviation */
  String getExpansion();
  List<Integer> getCountList();
  /** get the concept ids of the acronym or abbreviation */
  List<String> getCUIList();
}
