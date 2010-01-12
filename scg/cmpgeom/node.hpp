#ifndef _NODE_HPP
#define _NODE_HPP 1

class Node
{
protected:
  Node *_next;
  Node *_prev;
public:
  Node(void);
  virtual ~Node(void);
  Node *next(void);
  Node *prev(void);
  Node *insert(Node*);
  Node *remove(void);
  void splice(Node*);
};

#endif // _NODE_HPP
