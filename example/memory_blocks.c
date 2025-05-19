// #include "globals.h"
// #include "predicates.h"
// #include "fc_code.h"

typedef unsigned char u1; typedef unsigned short u2; typedef unsigned int u4;
typedef signed int s4;


// === Heap model ===
#define SEGM_SIZE 10000
#define MAX_OBJS  500
u2 ObjHeader[SEGM_SIZE];   // Object headers area
//Header(5x2B),Offset:Contents: 0:Owner,1:Flags,2:Class,3:BodyOffset,4:BodySize
u1  PersiData[SEGM_SIZE];  // Persistent objects data area
u1  TransData[SEGM_SIZE];  // Transient  objects data area

/*@ ghost // === Companion ghost memory view ===
  u4 gNumObjs;             // Number of allocated objects
  u1 gIsTrans  [MAX_OBJS]; // Nonzero for transient object
  u4 gHeadStart[MAX_OBJS]; // Start offset of object header
  u4 gDataStart[MAX_OBJS]; // Start offset of object data
  u4 gDataEnd  [MAX_OBJS]; // End offset of object data
  u4 gCurObj;              // Currently considered object number
  u4 gCurObj_src;              // Currently considered object number
  u4 gCurObj_dst;              // Currently considered object number
*/

/*@
predicate valid_heap_model = 
  0 <= gNumObjs <= MAX_OBJS &&
// headers of allocated objects are within ObjHeader segment
  (\forall integer i; 0 <= i < gNumObjs ==> 
    0 <= gHeadStart[i] <= SEGM_SIZE - 5 ) && 
// no overlapping between headers (each header has 5 u2's)
  (\forall integer i,j; 0 <= i < j < gNumObjs ==> 
    (gHeadStart[i] >= gHeadStart[j]+5 || gHeadStart[j] >= gHeadStart[i]+5) ) &&
// IsTrans[i] encodes if i-th object's transient bit is set
  (\forall integer i; 0 <= i < gNumObjs ==> 
    ( gIsTrans[i] <==> (ObjHeader[gHeadStart[i] + 1] & 0x0008) ) ) &&
// data of allocated objects is within a data segment
  (\forall integer i; 0 <= i < gNumObjs ==> 
    gDataStart[i] == ObjHeader[gHeadStart[i] + 3] &&
    gDataEnd[i] == gDataStart[i] + ObjHeader[gHeadStart[i] + 4] - 1 &&
    0 <= gDataStart[i] < gDataEnd[i] < SEGM_SIZE ) && 
// no overlapping between persistent object data
  (\forall integer i,j; 0<=i<j<gNumObjs && !gIsTrans[i] && !gIsTrans[j] ==> 
    (gDataStart[i] > gDataEnd[j] || gDataStart[j] > gDataEnd[i]) ) &&
// no overlapping between transient object data
  (\forall integer i,j; 0 <= i < j < gNumObjs && gIsTrans[i] && gIsTrans[j] ==> 
    (gDataStart[i] > gDataEnd[j] || gDataStart[j] > gDataEnd[i]) ); 
*/


// Comparison function with ghost paramaters
/*@ requires valid_p_src: u32_src == ((*(FCG_p_src + 0) * 0x1000000 + *(FCG_p_src + 1) * 0x10000) + *(FCG_p_src + 2) * 0x100) + *(FCG_p_src + 3);
    requires valid_p_dst: u32_dst == ((*(FCG_p_dst + 0) * 0x1000000 + *(FCG_p_dst + 1) * 0x10000) + *(FCG_p_dst + 2) * 0x100) + *(FCG_p_dst + 3);
    terminates \true;
    exits \false;
    ensures res:
      \let CASE_eq_1 = (short)*(\old(FCG_p_src) + 0) == (short)*(\old(FCG_p_dst) + 0);
      \let CASE_lt_1 = (short)*(\old(FCG_p_src) + 0) < (short)*(\old(FCG_p_dst) + 0);
      \let CASE_gt_1 = (short)*(\old(FCG_p_src) + 0) > (short)*(\old(FCG_p_dst) + 0);
      \let CASE_eq_2 = (short)*(\old(FCG_p_src) + 1) == (short)*(\old(FCG_p_dst) + 1);
      \let CASE_lt_2 = (short)*(\old(FCG_p_src) + 1) < (short)*(\old(FCG_p_dst) + 1);
      \let CASE_gt_2 = (short)*(\old(FCG_p_src) + 1) > (short)*(\old(FCG_p_dst) + 1);
      \let CASE_eq_3 = (short)*(\old(FCG_p_src) + 2) == (short)*(\old(FCG_p_dst) + 2);
      \let CASE_lt_3 = (short)*(\old(FCG_p_src) + 2) < (short)*(\old(FCG_p_dst) + 2);
      \let CASE_gt_3 = (short)*(\old(FCG_p_src) + 2) > (short)*(\old(FCG_p_dst) + 2);
      \let CASE_eq_4 = (short)*(\old(FCG_p_src) + 3) == (short)*(\old(FCG_p_dst) + 3);
      \let CASE_lt_4 = (short)*(\old(FCG_p_src) + 3) < (short)*(\old(FCG_p_dst) + 3);
      \let CASE_gt_4 = (short)*(\old(FCG_p_src) + 3) > (short)*(\old(FCG_p_dst) + 3);
      \let CASE_eq = CASE_eq_1 && CASE_eq_2 && CASE_eq_3 && CASE_eq_4 <==> \result == 0;
      \let CASE_lt = CASE_lt_1 || (CASE_eq_1 && CASE_lt_2) || (CASE_eq_1 && CASE_eq_2 && CASE_lt_3) || (CASE_eq_1 && CASE_eq_2 && CASE_lt_3) || (CASE_eq_1 && CASE_eq_2 && CASE_eq_3 && CASE_lt_4) <==> \result == (u1)-1;
      \let CASE_gt = CASE_gt_1 || (CASE_eq_1 && CASE_gt_2) || (CASE_eq_1 && CASE_eq_2 && CASE_gt_3) || (CASE_eq_1 && CASE_eq_2 && CASE_gt_3) || (CASE_eq_1 && CASE_eq_2 && CASE_eq_3 && CASE_gt_4) <==> \result == (u1)1;
      CASE_eq && CASE_lt && CASE_gt;
    ensures res_eq: ((s4)u32_src == (s4)u32_dst) <==> (\result == (u1)0);
    ensures res_lt: ((s4)u32_src < (s4)u32_dst) <==> (\result == (u1)(-1));
    ensures res_gt: ((s4)u32_src > (s4)u32_dst) <==> (\result == (u1)1);
    assigns \result;
 */
static u1 compareFourSignedBytes_1(u4 u32_src, u4 u32_dst) /*@ ghost (u1 FCG_p_src[4], u1 FCG_p_dst[4]) */;


// Comparison function without ghost paramaters
/*@ 
    terminates \true;
    exits \false;
    ensures res_eq: ((s4)u32_src == (s4)u32_dst) <==> (\result == (u1)0);
    ensures res_lt: ((s4)u32_src < (s4)u32_dst) <==> (\result == (u1)(-1));
    ensures res_gt: ((s4)u32_src > (s4)u32_dst) <==> (\result == (u1)1);
    assigns \result;
 */
static u1 compareFourSignedBytes_2(u4 u32_src, u4 u32_dst);


// Logic function used to simplify the proof
/*@ logic unsigned char * ptr_from_idx_offset(u4 idx, u4 off) = &PersiData[gDataStart[idx] + off]; */


// This function illustrates two use cases:
// 1- Instantiation of universal quantifier needed beacause of Qed simplification
// 2- Ghost code needed to perform byte-wise signed comparision
/*@
    requires r1: 0 <= gCurObj_src < gNumObjs;
    requires r2: 0 <= gCurObj_dst < gNumObjs;
    requires r3: 0 <= u16_length <= gDataEnd[gCurObj_src] - gDataStart[gCurObj_src] + 1;
    requires r4: 0 <= u16_length <= gDataEnd[gCurObj_dst] - gDataStart[gCurObj_dst] + 1;
    requires r5: 0 <= SrcOff <= gDataEnd[gCurObj_src] - gDataStart[gCurObj_src];
    requires r6: 0 <= DestOff <= gDataEnd[gCurObj_dst] - gDataStart[gCurObj_dst];
    requires r7: 0 <= SrcOff + u16_length <= gDataEnd[gCurObj_src] - gDataStart[gCurObj_src] + 1;
    requires r8: 0 <= DestOff + u16_length <= gDataEnd[gCurObj_dst] - gDataStart[gCurObj_dst] + 1;
    // requires src: p_src == ptr_from_idx_offset(gCurObj_src, SrcOff); // allows to avoid instantiation
    // requires dst: p_dst == ptr_from_idx_offset(gCurObj_dst, DestOff); // allows to avoid instantiation
    requires src: p_src == &PersiData[gDataStart[gCurObj_src] + SrcOff]; // instantiation required. harder version due to Qed simplification
    requires dst: p_dst == &PersiData[gDataStart[gCurObj_dst] + DestOff]; // instantiation required. harder version due to Qed simplification
    requires vhm: valid_heap_model;
    ensures eq: \result == 0 <==> (\forall integer i; 0 <= i < u16_length ==> (short)*(p_src + i) == (short)*(p_dst + i));
    ensures lt: (\exists integer j;
      \let cond_lt = 0 <= j < u16_length && (short)*(p_src + j) < (short)*(p_dst + j);
      \let cond_eq = \forall integer i; 0 <= i < j ==> (short)*(p_src + i) == (short)*(p_dst + i);
      cond_lt && cond_eq) <==> (\result == (u1)(-1));
    ensures gt: (\exists integer j;
      \let cond_gt = 0 <= j < u16_length && (short)*(p_src + j) > (short)*(p_dst + j);
      \let cond_eq = \forall integer i; 0 <= i < j ==> (short)*(p_src + i) == (short)*(p_dst + i);
      cond_gt && cond_eq) <==> (\result == (u1)1);
    assigns \result;    
*/
short compareBlocks(unsigned char * p_src, unsigned char * p_dst, unsigned int u16_length) /*@ ghost (u4 SrcOff, u4 DestOff) */
{
  u4 u32_src;
  u4 u32_dst;
  u1 u8_result_temp;
  unsigned int u16_length_sec;
  u1 u8_result;
  u16_length_sec = u16_length;
  u8_result = (u1)0;
  /*@ loop invariant range_1: 0 <= \at(u16_length_sec,LoopEntry) - u16_length_sec <= \at(u16_length_sec,LoopEntry);
      loop invariant range_2: 0 <= u16_length_sec <= \at(u16_length_sec,LoopEntry);
      loop invariant eq: u8_result == 0 <==> (\forall integer i; u16_length_sec <= i < \at(u16_length_sec,LoopEntry) ==> (short)*(p_src + i) == (short)*(p_dst + i));
      loop invariant lt: (\exists integer j;
         \let cond_lt = u16_length_sec <= j < \at(u16_length_sec,LoopEntry) && (short)*(p_src + j) < (short)*(p_dst + j);
         \let cond_eq = \forall integer i; u16_length_sec <= i < j ==> (short)*(p_src + i) == (short)*(p_dst + i);
         cond_lt && cond_eq) <==> (u8_result == (u1)(-1));
      loop invariant gt: (\exists integer j;
            \let cond_gt = u16_length_sec <= j < \at(u16_length_sec,LoopEntry) && (short)*(p_src + j) > (short)*(p_dst + j);
            \let cond_eq = \forall integer i; u16_length_sec <= i < j ==> (short)*(p_src + i) == (short)*(p_dst + i);
            cond_gt && cond_eq) <==> (u8_result == (u1)1);
      loop invariant result_cases: u8_result == 0 || u8_result == 255 || u8_result == 1;
      loop assigns u16_length_sec, u32_src, u32_dst, u8_result_temp, u8_result;
      loop variant (var: u16_length_sec);
  */
  while (u16_length_sec != 0U) {
    /*@ ghost u1 FCG_p_src[4]; */
    /*@ ghost u1 FCG_p_dst[4]; */
    if ((u16_length_sec & 3U) == 0U) {
      /*@ assert is_mod4: u16_length_sec % 4 == 0; */ ;
      u16_length_sec -= 4U;
      u32_src = (((unsigned long)*(p_src + u16_length_sec + 3) + (unsigned long)*(p_src + u16_length_sec + 2) * 256UL) + (unsigned long)*(p_src + u16_length_sec + 1) * 65536UL) + (unsigned long)*(p_src + u16_length_sec + 0) * 16777216UL;
      u32_dst = (((unsigned long)*(p_dst + u16_length_sec + 3) + (unsigned long)*(p_dst + u16_length_sec + 2) * 256UL) + (unsigned long)*(p_dst + u16_length_sec + 1) * 65536UL) + (unsigned long)*(p_dst + u16_length_sec + 0) * 16777216UL;
      /*@ ghost FCG_p_src[0] = *((p_src + u16_length_sec) + 0); */
      /*@ ghost FCG_p_src[1] = *((p_src + u16_length_sec) + 1); */
      /*@ ghost FCG_p_src[2] = *((p_src + u16_length_sec) + 2); */
      /*@ ghost FCG_p_src[3] = *((p_src + u16_length_sec) + 3); */
      /*@ ghost FCG_p_dst[0] = *((p_dst + u16_length_sec) + 0); */
      /*@ ghost FCG_p_dst[1] = *((p_dst + u16_length_sec) + 1); */
      /*@ ghost FCG_p_dst[2] = *((p_dst + u16_length_sec) + 2); */
      /*@ ghost FCG_p_dst[3] = *((p_dst + u16_length_sec) + 3); */
    }
    else 
      if ((u16_length_sec & 1U) == 0U) {
        u2 tmp;
        u2 tmp_0;
        /*@ assert is_mod2: u16_length_sec % 2 == 0; */ ;
        u16_length_sec -= 2U;
        u32_src = (unsigned short)((int)((unsigned short)*(p_src + u16_length_sec + 1)) + (int)((unsigned short)*(p_src + u16_length_sec + 0)) * 256);
        u32_dst = (unsigned short)((int)((unsigned short)*(p_dst + u16_length_sec + 1)) + (int)((unsigned short)*(p_dst + u16_length_sec + 0)) * 256);
        /*@ ghost FCG_p_src[0] = (u1)0; */
        /*@ ghost FCG_p_src[1] = (u1)0; */
        /*@ ghost FCG_p_src[2] = *((p_src + u16_length_sec) + 0); */
        /*@ ghost FCG_p_src[3] = *((p_src + u16_length_sec) + 1); */
        /*@ ghost FCG_p_dst[0] = (u1)0; */
        /*@ ghost FCG_p_dst[1] = (u1)0; */
        /*@ ghost FCG_p_dst[2] = *((p_dst + u16_length_sec) + 0); */
        /*@ ghost FCG_p_dst[3] = *((p_dst + u16_length_sec) + 1); */
      }
      else {
        u16_length_sec --;
        u32_src = (u4)*(p_src + u16_length_sec);
        u32_dst = (u4)*(p_dst + u16_length_sec);
        /*@ ghost FCG_p_src[0] = (u1)0; */
        /*@ ghost FCG_p_src[1] = (u1)0; */
        /*@ ghost FCG_p_src[2] = (u1)0; */
        /*@ ghost FCG_p_src[3] = *((p_src + u16_length_sec) + 0); */
        /*@ ghost FCG_p_dst[0] = (u1)0; */
        /*@ ghost FCG_p_dst[1] = (u1)0; */
        /*@ ghost FCG_p_dst[2] = (u1)0; */
        /*@ ghost FCG_p_dst[3] = *((p_dst + u16_length_sec) + 0); */
      }
    u8_result_temp = compareFourSignedBytes_1(u32_src, u32_dst) /*@ ghost (FCG_p_src, FCG_p_dst) */;
    // u8_result_temp = compareFourSignedBytes_2(u32_src, u32_dst); // harder version without ghost code
    if (u32_src != u32_dst) u8_result = u8_result_temp;
  }
  return  (short)u8_result;
}


// This function illustrates one well-known use case:
// 1- Usage of offsets instead of pointers makes the proof easier (but requires verbose specification with offsets !)
/*@
    requires r1: u16_length % 4 == 0;
    requires r2: 0 <= gCurObj_src < gNumObjs;
    requires r3: 0 <= gCurObj_dst < gNumObjs;
    requires r4: 0 <= u16_length <= gDataEnd[gCurObj_src] - gDataStart[gCurObj_src] + 1;
    requires r5: 0 <= u16_length <= gDataEnd[gCurObj_dst] - gDataStart[gCurObj_dst] + 1;
    requires r6: \separated(p_src + (0 .. u16_length - 1), p_dst + (0 .. u16_length - 1));
    requires src: p_src == &PersiData[gDataStart[gCurObj_src] + 0];
    requires dst: p_dst == &PersiData[gDataStart[gCurObj_dst] + 0];
    requires vhm: valid_heap_model;
    terminates \true;
    exits \false; 
    assigns *(p_dst + (0 .. u16_length - 1));
    ensures copy: \forall integer i; 0 <= i < u16_length ==> (*(&PersiData[gDataStart[gCurObj_dst]] + i) == *(&PersiData[gDataStart[gCurObj_src]] + i));
    // ensures copy: \forall integer i; 0 <= i < u16_length ==> (*(p_dst + i) == *(p_src + i)); // harder version with pointers instead of offsets
*/
void copySeparatedBlocks(unsigned char * p_src, unsigned char * p_dst, u2 u16_length){
  p_dst += u16_length;
  p_src += u16_length;
  u2 u16_idx = u16_length;
  /*@
    loop invariant range: 0 <= u16_idx <= u16_length;
    loop invariant mod: u16_idx % 4 == 0;
    loop invariant copy: \forall integer i; 0 <= i < u16_length - u16_idx ==> (*(&PersiData[gDataStart[gCurObj_dst] + (u4)u16_idx] + i) == *(&PersiData[gDataStart[gCurObj_src] + (u4)u16_idx] + i)); // Instantiation of i with (i - 4) needed
    // loop invariant copy: \forall integer i; 0 <= i < u16_length - u16_idx - 1 ==> (*(p_dst + i) == *(p_src + i)); // harder version with pointers instead of offsets
    loop invariant vhm: valid_heap_model;
    loop invariant src: p_src == &PersiData[gDataStart[gCurObj_src] + (u4)u16_idx];
    loop invariant dst: p_dst == &PersiData[gDataStart[gCurObj_dst] + (u4)u16_idx];
    loop invariant src: p_src == \at(p_src, LoopEntry) - (u16_length - u16_idx);
    loop invariant dst: p_dst == \at(p_dst, LoopEntry) - (u16_length - u16_idx);
    loop invariant src: p_src == \at(p_src, Pre) + u16_idx;
    loop invariant dst: p_dst == \at(p_dst, Pre) + u16_idx;
    loop assigns *(ptr_from_idx_offset(gCurObj_dst, (u4)u16_idx) + (0 .. (u16_length - u16_idx) - 1)), u16_idx, p_src, p_dst;
    // loop assigns *(p_dst + (0 .. (u16_length - u16_idx) - 1)), u16_idx, p_src, p_dst; // harder version with pointers instead of offsets
    loop variant var: u16_idx;
  */
while (u16_idx != 0) {
  u16_idx -= 4;
  p_dst -= 4;
  p_src -= 4;
 
 *(u1 *)(p_dst + 0) = *(u1 *)(p_src + 0);
 *(u1 *)(p_dst + 1) = *(u1 *)(p_src + 1);
 *(u1 *)(p_dst + 2) = *(u1 *)(p_src + 2);
 *(u1 *)(p_dst + 3) = *(u1 *)(p_src + 3);
}
}
