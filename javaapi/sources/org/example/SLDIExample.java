package org.example;

import gov.nih.nlm.nls.metamap.*;
import java.io.*;
import java.util.List;
import java.util.ArrayList;

/**
 * Describe class SLDIExample here.
 *  Single Line Delimited Input Example.
 *
 * Created: Thu May 23 15:01:22 2013
 *
 * @author <a href="mailto:wjrogers@mail.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class SLDIExample {

  String input = 
    "The results suggest that in smooth muscle induction of NO pathway relaxation, which is in part mediated by K+ channels and inducible NO synthase, may be of importance to the understanding of ischemia/reperfusion responses in cold-stored arteries.\n\n" +

"We constructed a model of peripheral nerve messages in an attempt to represent and quantitate the desynchronizations produced by cold and crush damage lesions in peripheral nerve messages.\n\n" +

"In this paper we deepen our understanding of cold denaturation by taking advantage of the theoretical model developed by Ikegami and using thermodynamic data on the transfer to water of liquid N-alkyl amides.\n\n" +

"Thus, the scintigraphy pattern of a hot spot in the bone scan and a cold lesion in the bone marrow scintigraphy is highly suggestive of a mandibular metastasis, if accompanied by anesthesia of the lower lip.\n\n" +

"Two-dimensional gel analysis showed that protein synthesis was deregulated in csp double mutants and that the loss of one or two CSPs led to an increase in the synthesis of the remaining CSP(s) at 37 degrees C and after cold shock, suggesting that CSPs down-regulate production of members from this protein family.\n\n" +

"Group I comprised 9 dogs submitted to renal autotransplantation; group II comprised 6 dogs submitted to renal autotransplantation after 24 h cold ischemia; group III comprised 6 dogs submitted to renal autotransplantation after 24 h cold ischemia and subsequent warm ischemia of 30 min; group IV comprised 9 dogs submitted to renal autotransplantation after 24 h cold ischemia and 60 min warm ischemia.\n\n" +

"Personal histories of hypertension and thyroid disease, and susceptibility to colds appeared to be positively associated with the risk (0.05 < P < 0.10).\n\n" +

"Relationship between cold tolerance and generation of suppressor macrophages during acute cold stress.\n\n" +

"Furthermore, both the clustering in coated pits and the co-precipitation with alpha-adaptin were dramatically reduced in the cold, suggesting that low temperature can interfere with the sorting of proteins into coated pits.\n\n" +

"When the cementation process was completed the samples were cycled 300 times between a 55 degrees C hot bath and a 5 degrees C cold bath.\n\n" +

"Colds, rashes, episodes of vomiting, ear infections, colic, and health care utilization were less frequent for breastfed infants.\n\n" +

"Polymerase chain reaction (PCR) using two sets of universal primers for bacterial 16S rDNA was performed on urine from the cystoscope and on a cold cup bladder biopsy specimen.\n\n" +

    "Chronic cold stress alters the basal and evoked electrophysiological activity of rat locus coeruleus neurons.\n\n";


  /**
   * Creates a new <code>SLDIExample</code> instance.
   *
   */
  public SLDIExample() {

  }

  void process(String outputFilename)
  {
    List<String> theOptions = new ArrayList<String>();
    try {
      MetaMapApi api = new MetaMapApiImpl();
      api.setTimeout(0);
      api.setOptions("--sldi");
      String options = api.getOptions();
      System.out.println("Options: " + options);
      List<Result> resultList = api.processCitationsFromString(this.input);
      PrintWriter pw = new PrintWriter(outputFilename);
      for (Result result: resultList) {
	if (result != null) {
	  List<AcronymsAbbrevs> aaList = result.getAcronymsAbbrevsList();
	  pw.println("Acronyms and Abbreviations:");
	  if (aaList.size() > 0) {
	    for (AcronymsAbbrevs e: aaList) {
	      pw.println("Acronym: " + e.getAcronym());
	      pw.println("Expansion: " + e.getExpansion());
	      pw.println("Count list: " + e.getCountList());
	      pw.println("CUI list: " + e.getCUIList());
	    }
	  } else {
	    pw.println(" None.");
	  }

	  pw.println("Negations:");
	  List<Negation> negList = result.getNegationList();
	  if (negList.size() > 0) {
	    for (Negation e: negList) {
	      pw.println("type: " + e.getType());
	      pw.print("Trigger: " + e.getTrigger() + ": [");
	      for (Position pos: e.getTriggerPositionList()) {
		pw.print(pos  + ",");
	      }
	      pw.println("]");
	      pw.print("ConceptPairs: [");
	      for (ConceptPair pair: e.getConceptPairList()) {
		pw.print(pair + ",");
	      }
	      pw.println("]");
	      pw.print("ConceptPositionList: [");
	      for (Position pos: e.getConceptPositionList()) {
		pw.print(pos + ",");
	      }
	      pw.println("]");
	    }
	  } else {
	    pw.println(" None.");
	  }
	  for (Utterance utterance: result.getUtteranceList()) {
	    pw.println("Utterance:");
	    pw.println(" Id: " + utterance.getId());
	    pw.println(" Utterance text: " + utterance.getString());
	    pw.println(" Position: " + utterance.getPosition());
	  
	    for (PCM pcm: utterance.getPCMList()) {
	      pw.println("Phrase:");
	      pw.println(" text: " + pcm.getPhrase().getPhraseText());

	      pw.println("Candidates:");
	      for (Ev ev: pcm.getCandidateList()) {
		pw.println(" Candidate:");
		pw.println("  Score: " + ev.getScore());
		pw.println("  Concept Id: " + ev.getConceptId());
		pw.println("  Concept Name: " + ev.getConceptName());
		pw.println("  Preferred Name: " + ev.getPreferredName());
		pw.println("  Matched Words: " + ev.getMatchedWords());
		pw.println("  Semantic Types: " + ev.getSemanticTypes());
		pw.println("  is Head?: " + ev.isHead());
		pw.println("  is Overmatch?: " + ev.isOvermatch());
		pw.println("  Sources: " + ev.getSources());
		pw.println("  Positional Info: " + ev.getPositionalInfo());
	      }
	      pw.println("Mappings:");
	      for (Mapping map: pcm.getMappingList()) {
		pw.println(" Mapping:");
		pw.println("  Map Score: " + map.getScore());
		for (Ev mapEv: map.getEvList()) {
		  pw.println("   Score: " + mapEv.getScore());
		  pw.println("   Concept Id: " + mapEv.getConceptId());
		  pw.println("   Concept Name: " + mapEv.getConceptName());
		  pw.println("   Preferred Name: " + mapEv.getPreferredName());
		  pw.println("   Matched Words: " + mapEv.getMatchedWords());
		  pw.println("   Semantic Types: " + mapEv.getSemanticTypes());
		  pw.println("   is Head?: " + mapEv.isHead());
		  pw.println("   is Overmatch?: " + mapEv.isOvermatch());
		  pw.println("   Sources: " + mapEv.getSources());
		  pw.println("   Positional Info: " + mapEv.getPositionalInfo());
		}
	      }
	    }
	  }
	}
      }
      pw.close();
    } catch (Exception e) {
      System.out.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  public static void main(String[] args) {
    SLDIExample  frontEnd =
      new SLDIExample();
    if (args.length < 1) {
      System.out.println("usage: SLDIExample <outputfile>");
      System.exit(0);
    }

    frontEnd.process(args[0]);
  }


}

