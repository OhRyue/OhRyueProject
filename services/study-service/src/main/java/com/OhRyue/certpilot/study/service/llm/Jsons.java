package com.OhRyue.certpilot.study.service.llm;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.*;

@SuppressWarnings("unchecked")
public final class Jsons {
  private static final ObjectMapper M = new ObjectMapper();
  private Jsons(){}

  public static String extractChoiceContent(Map<?,?> resp){
    if (resp == null) return "{}";
    var choices = (List<Map<String,Object>>) resp.get("choices");
    if (choices==null || choices.isEmpty()) return "{}";
    var msg = (Map<String,Object>) choices.get(0).get("message");
    return msg!=null ? String.valueOf(msg.getOrDefault("content","{}")) : "{}";
    // OpenAI 응답 포맷 기준
  }

  public static Map<String,Object> parseJsonObject(String s){
    try { return M.readValue(s, Map.class); }
    catch(Exception e){ return Map.of(); }
  }

  public static Map<String,Object> optMap(Map<?,?> m, String k){
    Object v = m==null? null : m.get(k);
    return v instanceof Map ? (Map<String,Object>) v : Map.of();
  }
  public static String optString(Map<?,?> m, String k){
    Object v = m==null? null : m.get(k);
    return v==null? null : String.valueOf(v);
  }
  public static Integer optInt(Map<?,?> m, String k){
    Object v = m==null? null : m.get(k);
    if (v instanceof Number n) return n.intValue();
    try { return v==null? null : Integer.parseInt(String.valueOf(v)); } catch(Exception e){ return null; }
  }
  public static Boolean optBoolean(Map<?,?> m, String k){
    Object v = m==null? null : m.get(k);
    if (v instanceof Boolean b) return b;
    if (v instanceof String s) {
      return "true".equalsIgnoreCase(s) || "1".equals(s);
    }
    if (v instanceof Number n) return n.intValue() != 0;
    return null;
  }
  public static Double optDouble(Map<?,?> m, String k){
    Object v = m==null? null : m.get(k);
    if (v instanceof Number n) return n.doubleValue();
    try { return v==null? null : Double.parseDouble(String.valueOf(v)); } catch(Exception e){ return null; }
  }
  public static List<String> optListString(Map<?,?> m, String k){
    Object v = m==null? null : m.get(k);
    if (v instanceof List<?> l){
      List<String> out = new ArrayList<>();
      for (Object o : l) out.add(String.valueOf(o));
      return out;
    }
    return List.of();
  }
}
