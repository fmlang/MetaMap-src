package gov.nih.nlm.nls.metamap;

import java.util.List;

/**
 * MetaMapApiTest Implementation Test Class.
 * 
 * <p>
 * Created: Wed May 20 15:54:08 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class MetaMapApiTest {

  /**
   * Creates a new <code>MetaMapApiTest</code> instance.
   *
   */
  public MetaMapApiTest() {

  }

  /**
   * Process terms using MetaMap API and display result to standard output.
   * @param terms input terms
   */
  void process(String terms) 
    throws Exception
  {
    MetaMapApi api = new MetaMapApiImpl();
    api.setOptions("qy");
    String options = api.getOptions();
    System.out.println("Options: " + options);
    Result result = api.processString(terms);
    if (result != null) {
      List<AcronymsAbbrevs> aaList = result.getAcronymsAbbrevs();
      System.out.println("Acronyms and Abbreviations:");
      if (aaList.size() > 0) {
	for (AcronymsAbbrevs e: aaList) {
	  System.out.println("Acronym: " + e.getAcronym());
	  System.out.println("Expansion: " + e.getExpansion());
	  System.out.println("Count list: " + e.getCountList());
	  System.out.println("CUI list: " + e.getCUIList());
	}
      } else {
	System.out.println(" None.");
      }
      System.out.println("Negations:");
      List<Negation> negList = result.getNegations();
      if (negList.size() > 0) {
	for (Negation e: negList) {
          System.out.println("type: " + e.getType());
	  System.out.print("Trigger: " + e.getTrigger() + ": [");
	  for (Position pos: e.getTriggerPositionList()) {
	    System.out.print(pos  + ",");
	  }
	  System.out.println("]");
	  System.out.print("ConceptPairs: [");
	  for (ConceptPair pair: e.getConceptPairList()) {
	    System.out.print(pair + ",");
	  }
	  System.out.println("]");
	  System.out.print("ConceptPositionList: [");
	  for (Position pos: e.getConceptPositionList()) {
	    System.out.print(pos + ",");
	  }
	  System.out.println("]");
	}
      } else {
	System.out.println(" None.");
      }
      for (Utterance utterance: result.getUtteranceList()) {
	System.out.println("Utterance:");
	System.out.println(" Id: " + utterance.getId());
	System.out.println(" Utterance text: " + utterance.getString());
	System.out.println(" Position: " + utterance.getPosition());

	for (PCM pcm: utterance.getPCMList()) {
	  System.out.println("Phrase:");
	  System.out.println(" text: " + pcm.getPhrase().getPhraseText());
	  System.out.println("Candidates:");
	  for (Ev ev: pcm.getCandidates()) {
	    System.out.println(" Candidate:");
	    System.out.println("  Score: " + ev.getScore());
	    System.out.println("  Concept Id: " + ev.getConceptId());
	    System.out.println("  Concept Name: " + ev.getConceptName());
	    System.out.println("  Preferred Name: " + ev.getPreferredName());
	    System.out.println("  Matched Words: " + ev.getMatchedWords());
	    System.out.println("  Semantic Types: " + ev.getSemanticTypes());
	    System.out.println("  is Head?: " + ev.isHead());
	    System.out.println("  is Overmatch?: " + ev.isOvermatch());
	    System.out.println("  Sources: " + ev.getSources());
	    System.out.println("  Positional Info: " + ev.getPositionalInfo());
	  }

	  System.out.println("Mappings:");
	  for (Map map: pcm.getMappings()) {
	    System.out.println(" Map Score: " + map.getScore());
	    for (Ev mapEv: map.getEvList()) {
	      System.out.println("   Score: " + mapEv.getScore());
	      System.out.println("   Concept Id: " + mapEv.getConceptId());
	      System.out.println("   Concept Name: " + mapEv.getConceptName());
	      System.out.println("   Preferred Name: " + mapEv.getPreferredName());
	      System.out.println("   Matched Words: " + mapEv.getMatchedWords());
	      System.out.println("   Semantic Types: " + mapEv.getSemanticTypes());
	      System.out.println("   is Head?: " + mapEv.isHead());
	      System.out.println("   is Overmatch?: " + mapEv.isOvermatch());
	      System.out.println("   Sources: " + mapEv.getSources());
	      System.out.println("   Positional Info: " + mapEv.getPositionalInfo());
	    }
	  }
	}
      }
    } else {
      System.out.println("NULL result instance! ");
    }
  }

  public static void main(String[] args) 
    throws Exception
  {
    MetaMapApiTest frontEnd = new MetaMapApiTest();
    
    if (args.length < 1) {
      System.out.println("usage: gov.nih.nlm.nls.metamap.MetaMapApiTest terms");
      System.exit(0);
    }
    StringBuffer sb = new StringBuffer();
    for (String arg: args) {
      sb.append(arg).append(" ");
    }
    frontEnd.process(sb.toString());
  }
}
