#include <gtk/gtk.h>
#include <stdio.h>

GClosure *closure = NULL;

void run() {
  GtkWidget *window;
  if(!closure) {
    puts("no closure!");
    return;
  }
  gtk_init(0, NULL);
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  g_signal_connect_closure(window, "delete_event", closure, 0);
  g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), 0);
  gtk_widget_show(window);
  gtk_main();
}
