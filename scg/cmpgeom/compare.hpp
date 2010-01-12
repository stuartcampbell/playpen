#ifndef _COMPARE_HPP
#define _COMPARE_HPP 1

/**
 * \brief This function compares two numbers and returns 0 (eq), 1 (gt) or
 * -1 (lt).
 *
 * This function takes two numbers on compares them. The function returns
 * 0 is the two numbers are equal, 1 if the first number is larger than the
 * second number, and -1 if the first number is smaller than the second
 * number.
 *
 * \param value1 (INPUT) is the first number to be compared
 * \param value2 (INPUT) is the second number to be compared
 *
 * \return 0 (eq), 1 (gt) or -1 (lt)
 */
int compare(const double & value1, const double & value2);

#endif //_COMPARE_HPP
