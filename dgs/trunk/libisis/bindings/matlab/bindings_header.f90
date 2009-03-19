#if defined(IXD_TYPE)

  use IXM&/**/
         &IXD_TYPE
  use IXMmatlab_interface
  implicit none
  interface IXBsendToBinding
      module procedure IXBsendToBinding&/**/
                                       &IXD_TYPE
      module procedure IXBsendToBindingArray&/**/
                                            &IXD_TYPE
  end interface
  interface IXBgetFromBinding
      module procedure IXBgetFromBinding&/**/
                                        &IXD_TYPE
      module procedure IXBgetFromBindingArray&/**/
                                             &IXD_TYPE
  end interface

#undef IXD_TYPE

#endif /* IXD_TYPE */

